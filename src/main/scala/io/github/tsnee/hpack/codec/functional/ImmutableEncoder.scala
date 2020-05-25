package io.github.tsnee.hpack.codec.functional

import io.github.tsnee.hpack._
import io.github.tsnee.hpack.codec._
import io.github.tsnee.hpack.table.{Indexing, Match}
import zio.Chunk

import scala.annotation.tailrec

private object ImmutableEncoder extends Encoder {
  override def encode(
    headerList: Seq[HeaderField],
    ctx: EncoderContext
  ): (Chunk[Byte], EncoderContext) =
    ctx match {
      case ictx: ImmutableEncoderContext =>
        val newCtx = headerList.foldLeft(ictx)(encodeOneField)
        (newCtx.headerBlock, newCtx)
      case _ =>
        sys.error("This Encoder cannot work with that EncoderContext.")
    }

  override private[codec] def encodePositiveInt(
    header: Byte,
    prefixLen: Int,
    i: Int
  ): Chunk[Byte] = {
    assert(i > 0)
    assert(prefixLen > 0)
    assert(prefixLen <= 8)
    val maxPrefix = 0xFF >>> (8 - prefixLen)
    if (i < maxPrefix)
      Chunk.single(header | i)
    else
      Chunk.fromIterable(encodeIntRecursive(i - maxPrefix, List(header | maxPrefix)))
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
        (0x80 | (0x0000007F & remainder)) :: encoded)
  }

  private def encodeOneField(
    acc: ImmutableEncoderContext,
    hf: HeaderField
  ): ImmutableEncoderContext = {
    val index = acc.indexed.get(hf.name) match {
      case Some(Indexing.Without) => false
      case Some(Indexing.Never) => false
      case _ => true
    }
    val encodedHeader =
      if (index) {
        acc.table.find(hf) match {
          case Match.Full(idx) =>
            val header = IndexedHeaderField.header(Indexing.With)
            encodePositiveInt(header, 7, idx)
          case Match.Partial(idx) =>
            val header = LiteralHeaderField.header(Indexing.With)
            encodePositiveInt(header, 7, idx)
            //encodeValue
          case Match.NotFound =>
            val header = LiteralHeaderField.header(Indexing.With)
            encodePositiveInt(header, 7, 0)
          //encodeValue
        }
        val (name, value) = if (
          (!acc.compressedByDefault && acc.compressed.contains(hf.name)) ||
            (acc.compressedByDefault && !acc.notCompressed.contains(hf.name))
        )
          compress(hf)
        else
          (hf.name, hf.value)
      }
    ???
  }

  private def compress(field: HeaderField) = ???
}
