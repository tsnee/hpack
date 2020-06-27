package io.github.tsnee.hpack.codec.functional

import scala.annotation.tailrec
import io.github.tsnee.hpack._
import io.github.tsnee.hpack.codec._
import io.github.tsnee.hpack.huffman.HuffmanCodec
import io.github.tsnee.hpack.table.{DynamicTable, Indexing, Match}
import zio.Chunk

private[codec] case class ImmutableEncoderContext(
  table: DynamicTable,
  indexedFields: Map[Chunk[Byte], Indexing] =
    Map.empty.withDefaultValue(Indexing.With),
  compressedByDefault: Boolean = true,
  compressedFields: Set[Chunk[Byte]] = Set.empty,
  uncompressedFields: Set[Chunk[Byte]] = Set.empty,
  encoded: Chunk[Byte] = Chunk.empty
) extends EncoderContext {
  override def headerBlock: (Chunk[Byte], EncoderContext) =
    (encoded, copy(encoded = Chunk.empty))

  override def index(fieldMapping: Map[String, Indexing]): EncoderContext =
    copy(indexedFields = fieldMapping.map {
      case (k, v) => stringToByteChunk(k) -> v
    })

  override def compressByDefault(b: Boolean): EncoderContext =
    copy(compressedByDefault = b)

  override def compress(fields: Set[String]): EncoderContext =
    copy(compressedFields = fields.map(s => stringToByteChunk(s)))

  override def doNotCompress(fields: Set[String]): EncoderContext =
    copy(uncompressedFields = fields.map(s => stringToByteChunk(s)))
}

object ImmutableEncoderContext {
  def apply(tableSize: Int): ImmutableEncoderContext =
    ImmutableEncoderContext(DynamicTable(tableSize))
}

private[hpack] object ImmutableEncoder extends Encoder {
  override def encode(
    headerList: Seq[HeaderField],
    ctx: EncoderContext
  ): EncoderContext =
    ctx match {
      case ictx: ImmutableEncoderContext =>
        headerList.foldLeft(ictx)(encodeOneField)
      case _ =>
        sys.error("This Encoder cannot work with that EncoderContext.")
    }

  private[codec] def encodeNonNegativeInt(
    header: Byte,
    prefixLen: Int,
    i: Int
  ): Chunk[Byte] = {
    assert(i >= 0)
    assert(prefixLen > 0)
    assert(prefixLen <= 8)
    val maxPrefix = 0xFF >>> (8 - prefixLen)
    if (i < maxPrefix)
      Chunk.single(header | i)
    else
      Chunk.fromIterable(
        encodeIntRecursive(i - maxPrefix, List(header | maxPrefix))
      )
  }

  @tailrec
  private def encodeIntRecursive(
    remainder: Int,
    encoded: List[Byte]
  ): List[Byte] = {
    assert(remainder >= 0)
    if (remainder < Byte.MaxValue)
      (remainder.toByte :: encoded).reverse
    else
      encodeIntRecursive(
        remainder >>> 7,
        (0x80 | (0x0000007F & remainder)) :: encoded
      )
  }

  private def encodeOneField(
    acc: ImmutableEncoderContext,
    hf: HeaderField
  ): ImmutableEncoderContext = {
    val indexing = acc.indexedFields.getOrElse(hf.name, Indexing.With)
    acc.table.find(hf) match {
      case Match.Full(idx) =>
        val (header, len) = IndexedHeaderField.header(indexing)
        val encHdr = encodeNonNegativeInt(header, 8 - len, idx)
        //Console.err.println(s"Encoded indexed ($idx) $hf as ${encHdr.map(_.toHexString)}.")
        acc.copy(encoded = acc.encoded ++ encHdr)
      case Match.Partial(idx) =>
        //Console.err.println(s"Partial match of $hf at $idx.")
        val (header, len) = LiteralHeaderField.header(indexing)
        //Console.err.println(s"Using header $header and len $len.")
        val c = shouldCompress(acc, hf.name)
        val encHdr = encodeNonNegativeInt(header, 8 - len, idx) ++
          encodeString(c, hf.value)
        //Console.err.println(s"Encoded partially-indexed $hf as ${encHdr.map(_.toHexString)}.")
        acc.copy(
          encoded = acc.encoded ++ encHdr,
          table = acc.table.store(hf, indexing))
      case Match.NotFound =>
        val (header, len) = LiteralHeaderField.header(indexing)
        val c = shouldCompress(acc, hf.name)
        val encHdr = encodeNonNegativeInt(header, 8 - len, 0) ++
          encodeString(c, hf.name) ++ encodeString(c, hf.value)
        //Console.err.println(s"Encoded unindexed $hf as ${encHdr.map(_.toHexString)}.")
        acc.copy(
          encoded = acc.encoded ++ encHdr,
          table = acc.table.store(hf, indexing))
    }
  }

  private def shouldCompress(
    ctx: ImmutableEncoderContext,
    name: Chunk[Byte]
  ): Boolean =
    (!ctx.compressedByDefault && ctx.compressedFields.contains(name)) ||
      (ctx.compressedByDefault && !ctx.uncompressedFields.contains(name))

  private def encodeString(
    compress: Boolean,
    raw: Chunk[Byte]
  ): Chunk[Byte] = {
    val processed = if (compress)
      HuffmanCodec.default.encode(raw)
    else
      raw
    val header = if (compress)
      0x80
    else
      0x00
    val length = encodeNonNegativeInt(header, 7, processed.size)
    length ++ processed
  }
}
