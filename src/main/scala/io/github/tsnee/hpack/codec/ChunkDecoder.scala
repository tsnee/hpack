package io.github.tsnee.hpack.codec

import scala.annotation.tailrec
import scala.language.implicitConversions
import io.github.tsnee.hpack.{HpackError, HeaderField}
import io.github.tsnee.hpack.huffman.HuffmanCodec
import io.github.tsnee.hpack.table.{Indexing, StaticTable}
import zio.Chunk

private object ChunkDecoder extends Decoder {
  implicit def forConvenience(i: Int): Byte = i.toByte

  override def decode(
    chunk: Chunk[Byte],
    ctx: DecoderContext
  ): Either[HpackError, DecoderContext] = ctx match {
    case chunkCtx: ChunkDecoderContext =>
      chunkCtx.bytes = chunkCtx.bytes ++ chunk
      decodeRecursive(chunkCtx)
      if (chunkCtx.error.isEmpty)
        Right(chunkCtx)
      else
        Left(chunkCtx.error.get)
    case _ => Left(
      HpackError.Implementation(
        "This Decoder implementation does not work with this type of DecoderContext."
      )
    )
  }

  @tailrec
  private def decodeRecursive(
    ctx: ChunkDecoderContext
  ): Unit =
    if (ctx.bytes.isDefinedAt(ctx.offset)) {
      val origHeaders = ctx.headers
      val origOffset = ctx.offset
      val head = ctx.bytes.byte(origOffset)
      if ((head & 0x80) != 0x00)
        indexedHeader(ctx)
      else if (head == 0x40) {
        ctx.offset = origOffset + 1
        literalHeaderNewName(ctx, Indexing.With)
      }
      else if ((head & 0xC0) == 0x40)
        literalHeaderIndexedName(ctx, 0x3F, Indexing.With)
      else if (head == 0x00) {
        ctx.offset = origOffset + 1
        literalHeaderNewName(ctx, Indexing.Without)
      }
      else if ((head & 0xF0) == 0x00)
        literalHeaderIndexedName(ctx, 0x0F, Indexing.Without)
      else if (head == 0x10) {
        ctx.offset = origOffset + 1
        literalHeaderNewName(ctx, Indexing.Never)
      }
      else if ((head & 0xF0) == 0x10)
        literalHeaderIndexedName(ctx, 0x0F, Indexing.Never)
      else if ((head & 0xE0) != 0x00)
        resizeTable(ctx, 0x1F)
      else {
        val error = HpackError.InvalidInput(
          "Could not parse header block.",
          origOffset,
          HpackError.Expectation.FirstHeaderByte,
          head
        )
        ctx.error = Some(error)
      }
      if (ctx.headers != origHeaders && ctx.offset > origOffset)
        decodeRecursive(ctx)
      else if (ctx.error.isEmpty)
        ctx.offset = origOffset
    }

  /** See RFC 7541 section 6.1. */
  private def indexedHeader(
    ctx: ChunkDecoderContext
  ): Unit =
    decodeInt(0x7F, ctx.bytes, ctx.offset) match {
      case Right((idx, afterIdx)) =>
        ctx.table.lookup(idx) match {
          case Some(headerField) =>
            ctx.headers = headerField :: ctx.headers
            ctx.offset = afterIdx
          case None => tableLookupFailure(ctx, idx)
        }
      case Left(err: HpackError.InvalidInput) => decodeHeaderIndexFailure(ctx, err)
      case Left(err: HpackError.Implementation) => ctx.error = Some(err)
      case Left(_: HpackError.IncompleteInput) => // Do nothing
    }

  private def decodeHeaderIndexFailure(
    ctx: ChunkDecoderContext,
    chained: HpackError
  ): Unit =
    ctx.error = Some(
      HpackError.InvalidInput(
        "Cannot decode header index",
        ctx.offset,
        HpackError.Expectation.HeaderIndex,
        ctx.bytes.byte(ctx.offset),
        Some(chained)
      )
    )

  private def literalHeaderNewName(
    ctx: ChunkDecoderContext,
    indexing: Indexing
  ): Unit =
    decodeString(ctx) match {
      case Right((name, afterName)) =>
        //Console.err.println(s"name ${new String(name.toArray)}")
        ctx.offset = afterName
        decodeValue(name, ctx, indexing)
      case Left(err: HpackError.InvalidInput) =>
       /* Console.err.println(s"name error $err"); */
        ctx.error = Some(
          HpackError.InvalidInput(
            "Cannot decode header name starting with " +
              ctx.bytes.byte(ctx.offset) + ".",
            ctx.offset,
            HpackError.Expectation.HeaderName,
            ctx.bytes.byte(ctx.offset),
            Some(err)
          )
        )
      case Left(err: HpackError.Implementation) => ctx.error = Some(err)
      case Left(_: HpackError.IncompleteInput) => // Nothing to do
  }

  private def decodeValue(
    name: Chunk[Byte],
    ctx: ChunkDecoderContext,
    indexing: Indexing
  ): Unit =
    decodeString(ctx) match {
      case Right((value, afterValue)) =>
        //Console.err.println(s"name ${new String(name.toArray)} value ${new String(value.toArray)} indexing $indexing")
        val headerField = HeaderField(name, value)
        ctx.headers = headerField :: ctx.headers
        ctx.offset = afterValue
        ctx.table = ctx.table.store(headerField, indexing)
      case Left(err: HpackError.InvalidInput) => /* Console.err.println(s"value error $err"); */
        ctx.error = Some(
          HpackError.InvalidInput(
            "Cannot decode header value starting with " +
              ctx.bytes.byte(ctx.offset) + ".",
            ctx.offset,
            HpackError.Expectation.HeaderValue,
            ctx.bytes.byte(ctx.offset),
            Some(err)
          )
        )
      case Left(err: HpackError.Implementation) => ctx.error = Some(err)
      case Left(_: HpackError.IncompleteInput) => // Nothing to do
    }

  private def literalHeaderIndexedName(
    ctx: ChunkDecoderContext,
    mask: Byte,
    indexing: Indexing
  ): Unit =
    decodeInt(mask, ctx.bytes, ctx.offset) match {
      case Right((idx, afterIdx)) =>
        ctx.table.lookup(idx) match {
          case Some(HeaderField(name, _)) =>
            //Console.err.println(s"Looked up ${new String(name.toArray)}")
            ctx.offset = afterIdx
            decodeValue(name, ctx, indexing)
          case None => tableLookupFailure(ctx, idx)
        }
      case Left(err: HpackError.InvalidInput) => decodeHeaderIndexFailure(ctx, err)
      case Left(err: HpackError.Implementation) => ctx.error = Some(err)
      case Left(_: HpackError.IncompleteInput) => // Nothing to do
    }

  private def tableLookupFailure(
    ctx: ChunkDecoderContext,
    idx: Int
  ): Unit = {
    val maxIdx = StaticTable.numEntries + ctx.table.numEntries
    ctx.error = Some(
      HpackError.InvalidInput(
        s"Invalid header index $idx is not between 1 and $maxIdx.",
        ctx.offset,
        HpackError.Expectation.HeaderIndex,
        ctx.bytes.byte(ctx.offset)
      )
    )
  }

  private def resizeTable(
    ctx: ChunkDecoderContext,
    mask: Byte
  ): Unit =
    decodeInt(mask, ctx.bytes, ctx.offset) match {
      case Right((newSize, newOffset)) =>
        ctx.table = ctx.table.resize(newSize)
        ctx.offset = newOffset
      case Left(err: HpackError.InvalidInput) =>
        ctx.error = Some(
          HpackError.InvalidInput(
            "Cannot decode table size parameter starting with " +
              ctx.bytes.byte(ctx.offset) + ".",
            ctx.offset,
            HpackError.Expectation.NonZeroLength,
            ctx.bytes.byte(ctx.offset),
            Some(err)
          )
        )
      case Left(err: HpackError.Implementation) => ctx.error = Some(err)
      case Left(_: HpackError.IncompleteInput) => // Nothing to do
    }

  /** See RFC 7541 section 5.1. */
  private[hpack] def decodeInt(
    mask: Byte,
    bytes: Chunk[Byte],
    offset: Int
  ): Either[HpackError, (Int, Int)] =
    if (bytes.isDefinedAt(offset)) {
      val value = bytes.byte(offset) & mask
      if (value != mask)  // i.e. not all 1s
        Right((value, offset + 1))
      else
        decodeIntRecursive(value, bytes, offset + 1, 0)
    }
    else
      Left(HpackError.IncompleteInput(offset))

  @tailrec
  private def decodeIntRecursive(
    value: Int,
    bytes: Chunk[Byte],
    offset: Int,
    m: Int
  ): Either[HpackError, (Int, Int)] =
    if (!bytes.isDefinedAt(offset)) {
      //Console.err.println(s"decodeIntRecursive($value, $bytes, $offset, $m)")
      Left(HpackError.IncompleteInput(offset))
    }
    else if (m > 21)
      Left(HpackError.Implementation("Cannot handle a header size this big."))
    else {
      val b = bytes.byte(offset)
      val i = value + ((b & 0x7F) << m)
      if ((b & 0x80) == 0x00)
        Right((i, offset + 1))
      else
        decodeIntRecursive(i, bytes, offset + 1, m + 7)
    }

  /** See RFC 7541 section 5.2. */
  private[hpack] def decodeString(
    ctx: ChunkDecoderContext
  ): Either[HpackError, (Chunk[Byte], Int)] =
    decodeHuffman(ctx).flatMap { h =>
      decodeInt(0x7F, ctx.bytes, ctx.offset) match {
        case Right((strSize, strOffset)) =>
          if (strOffset + strSize > ctx.bytes.size)
            Left(HpackError.IncompleteInput(strOffset))
          else
            Right((readString(ctx, h, strOffset, strSize), strOffset + strSize))
        case Left(err: HpackError.InvalidInput) => Left(
          HpackError.InvalidInput(
            "Cannot decode string length.",
            ctx.offset,
            HpackError.Expectation.NonZeroLength,
            ctx.bytes.byte(ctx.offset),
            Some(err)
          )
        )
        case Left(err) => Left(err)
    }
  }

  private[hpack] def decodeHuffman(
    ctx: ChunkDecoderContext
  ): Either[HpackError, Boolean] =
    ctx.bytes.lift(ctx.offset) match {
      case Some(h) => Right((h & 0x80) != 0x00)
      case None => Left(HpackError.IncompleteInput(ctx.offset))
    }

  private def readString(
    ctx: ChunkDecoderContext,
    h: Boolean,
    strOffset: Int,
    strSize: Int
  ): Chunk[Byte] = {
    val raw = ctx.bytes.slice(strOffset, strOffset + strSize)
    if (h)
      HuffmanCodec.default.decode(raw)
    else
      Chunk.fromIterable(raw)
  }
}
