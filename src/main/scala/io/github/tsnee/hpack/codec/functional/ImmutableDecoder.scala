package io.github.tsnee.hpack.codec.functional

import io.github.tsnee.hpack._
import io.github.tsnee.hpack.codec.{Decoder, DecoderContext}
import io.github.tsnee.hpack.huffman.HuffmanCodec
import io.github.tsnee.hpack.table.{DynamicTable, Indexing, StaticTable}
import zio.Chunk

import scala.annotation.tailrec

private[codec] case class ImmutableDecoderContext(
  table: DynamicTable,
  bytes: Chunk[Byte] = Chunk.empty,
  offset: Int = 0,
  headers: List[HeaderField] = Nil,
  error: Option[HpackError] = None
) extends DecoderContext {
  override def headerList: (Seq[HeaderField], DecoderContext) = {
    assert(bytes.size == offset)  // all input consumed
    (headers.reverse, ImmutableDecoderContext(table))
  }
}

private[codec] object ImmutableDecoder extends Decoder {
  override def decode(
    chunk: Chunk[Byte],
    ctx: DecoderContext
  ): Either[HpackError, DecoderContext] = ctx match {
    case chunkCtx: ImmutableDecoderContext =>
      val newCtx = decodeRecursive(chunkCtx.copy(bytes = chunkCtx.bytes ++ chunk))
      if (newCtx.error.isEmpty)
        Right(newCtx)
      else
        Left(newCtx.error.get)
    case _ => Left(
      HpackError.Implementation(
        "This Decoder implementation does not work with this type of DecoderContext."
      )
    )
  }

  @tailrec
  private def decodeRecursive(
    ctx: ImmutableDecoderContext
  ): ImmutableDecoderContext = {
    val headOpt = ctx.bytes.lift(ctx.offset)
    val newCtxOpt =
      if (headOpt.isEmpty)
        None
      else if ((headOpt.get & 0x80) != 0x00)
        indexedHeader(ctx)
      else if (headOpt.get == 0x40)
        literalHeaderNewName(
          ctx.copy(offset = ctx.offset + 1),
          Indexing.With
        )
      else if ((headOpt.get & 0xC0) == 0x40)
        literalHeaderIndexedName(
          ctx,
          0x3F,
          Indexing.With
        )
      else if (headOpt.get == 0x00)
        literalHeaderNewName(
          ctx.copy(offset = ctx.offset + 1),
          Indexing.Without
        )
      else if ((headOpt.get & 0xF0) == 0x00)
        literalHeaderIndexedName(
          ctx,
          0x0F,
          Indexing.Without
        )
      else if (headOpt.get == 0x10)
        literalHeaderNewName(
          ctx.copy(offset = ctx.offset + 1),
          Indexing.Never
        )
      else if ((headOpt.get & 0xF0) == 0x10)
        literalHeaderIndexedName(
          ctx,
          0x0F,
          Indexing.Never
        )
      else if ((headOpt.get & 0xE0) != 0x00)
        resizeTable(ctx)
      else {
        val error = HpackError.InvalidInput(
          "Could not parse header block.",
          ctx.offset,
          HpackError.Expectation.FirstHeaderByte,
          headOpt.get
        )
        Some(ctx.copy(error = Some(error)))
      }
    newCtxOpt match {
      case Some(newCtx) if newCtx.error.isEmpty =>
        //Console.err.println(s"newCtx $newCtx")
        decodeRecursive(newCtx)
      case Some(newCtx) =>
        //Console.err.println(s"Parse error $newCtx")
        newCtx   // Parse error
      case None =>
        //Console.err.println("Ran out of input?")
        ctx              // Not enough input to parse one header
    }
  }

  /** See RFC 7541 section 6.1. */
  private def indexedHeader(
    ctx: ImmutableDecoderContext
  ): Option[ImmutableDecoderContext] =
    decodeInt(0x7F, ctx.bytes, ctx.offset) match {
      case Right((idx, afterIdx)) =>
        ctx.table.lookup(idx) match {
          case Some(headerField) => Some(
            ctx.copy(
              headers = headerField :: ctx.headers,
              bytes = ctx.bytes.drop(afterIdx),
              offset = 0
            )
          )
          case None => tableLookupFailure(ctx, idx)
        }
      case Left(err: HpackError.InvalidInput) => decodeHeaderIndexFailure(ctx, err)
      case Left(err: HpackError.Implementation) => Some(ctx.copy(error = Some(err)))
      case Left(_: HpackError.IncompleteInput) => None
    }

  private def decodeHeaderIndexFailure(
                                        ctx: ImmutableDecoderContext,
                                        chained: HpackError
  ): Option[ImmutableDecoderContext] =
    Some(
      ctx.copy(
        error = Some(
          HpackError.InvalidInput(
            "Cannot decode header index",
            ctx.offset,
            HpackError.Expectation.HeaderIndex,
            ctx.bytes(ctx.offset),
            Some(chained)
          )
        )
      )
    )

  private def literalHeaderNewName(
                                    ctx: ImmutableDecoderContext,
                                    indexing: Indexing
  ): Option[ImmutableDecoderContext] =
    decodeString(ctx) match {
      case Right((name, afterName)) =>
        //Console.err.println(s"name ${new String(name.toArray)}")
        decodeValue(name, ctx.copy(offset = afterName), indexing)
      case Left(err: HpackError.InvalidInput) => /* Console.err.println(s"name error $err"); */ Some(
        ctx.copy(
          error = Some(
            HpackError.InvalidInput(
              "Cannot decode header name starting with " +
                ctx.bytes(ctx.offset) + ".",
              ctx.offset,
              HpackError.Expectation.HeaderName,
              ctx.bytes(ctx.offset),
              Some(err)
            )
          )
        )
      )
      case Left(err: HpackError.Implementation) => Some(ctx.copy(error = Some(err)))
      case Left(_: HpackError.IncompleteInput) => None
  }

  private def decodeValue(
                           name: Chunk[Byte],
                           ctx: ImmutableDecoderContext,
                           indexing: Indexing
  ): Option[ImmutableDecoderContext] =
    decodeString(ctx) match {
      case Right((value, afterValue)) =>
        //Console.err.println(s"name ${new String(name.toArray)} value ${new String(value.toArray)} indexing $indexing")
        val headerField = HeaderField(name, value)
        Some(
          ctx.copy(
            headers = headerField :: ctx.headers,
            bytes = ctx.bytes.drop(afterValue),
            offset = 0,
            table = ctx.table.store(headerField, indexing)
          )
        )
      case Left(err: HpackError.InvalidInput) => /* Console.err.println(s"value error $err"); */ Some(
        ctx.copy(
          error = Some(
            HpackError.InvalidInput(
              "Cannot decode header value starting with " +
                ctx.bytes(ctx.offset) + ".",
              ctx.offset,
              HpackError.Expectation.HeaderValue,
              ctx.bytes(ctx.offset),
              Some(err)
            )
          )
        )
      )
      case Left(err: HpackError.Implementation) => Some(
        ctx.copy(
          error = Some(err)
        )
      )
      case Left(_: HpackError.IncompleteInput) => None
    }

  private def literalHeaderIndexedName(
                                        ctx: ImmutableDecoderContext,
                                        mask: Byte,
                                        indexing: Indexing
  ): Option[ImmutableDecoderContext] =
    decodeInt(mask, ctx.bytes, ctx.offset) match {
      case Right((idx, afterIdx)) =>
        ctx.table.lookup(idx) match {
          case Some(HeaderField(name, _)) =>
            //Console.err.println(s"Looked up ${new String(name.toArray)}")
            decodeValue(name, ctx.copy(offset = afterIdx), indexing)
          case None => tableLookupFailure(ctx, idx)
        }
      case Left(err: HpackError.InvalidInput) => decodeHeaderIndexFailure(ctx, err)
      case Left(err: HpackError.Implementation) => Some(ctx.copy(error = Some(err)))
      case Left(_: HpackError.IncompleteInput) => None
    }

  private def tableLookupFailure(
                                  ctx: ImmutableDecoderContext,
                                  idx: Int
  ): Option[ImmutableDecoderContext] = {
    val maxIdx = StaticTable.numEntries + ctx.table.numEntries
    Some(
      ctx.copy(
        error = Some(
          HpackError.InvalidInput(
            s"Invalid header index $idx is not between 1 and $maxIdx.",
            ctx.offset,
            HpackError.Expectation.HeaderIndex,
            ctx.bytes(ctx.offset)
          )
        )
      )
    )
  }

  private def resizeTable(
    ctx: ImmutableDecoderContext,
  ): Option[ImmutableDecoderContext] =
    decodeInt(0x1F, ctx.bytes, ctx.offset) match {
      case Right((newSize, newOffset)) => Some(
        ctx.copy(
          table = ctx.table.resize(newSize),
          bytes = ctx.bytes.drop(newOffset),
          offset = 0
        )
      )
      case Left(err: HpackError.InvalidInput) => Some(
        ctx.copy(
          error = Some(
            HpackError.InvalidInput(
              "Cannot decode table size parameter starting with " +
                ctx.bytes(ctx.offset) + ".",
              ctx.offset,
              HpackError.Expectation.NonZeroLength,
              ctx.bytes(ctx.offset),
              Some(err)
            )
          )
        )
      )
      case Left(err: HpackError.Implementation) => Some(ctx.copy(error = Some(err)))
      case Left(_: HpackError.IncompleteInput) => None
    }

  /** See RFC 7541 section 5.1. */
  private[codec] def decodeInt(
    mask: Byte,
    bytes: Chunk[Byte],
    offset: Int
  ): Either[HpackError, (Int, Int)] =
    if (bytes.isDefinedAt(offset)) {
      val value = bytes(offset) & mask
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
      val b = bytes(offset)
      val i = value + ((b & 0x7F) << m)
      if ((b & 0x80) == 0x00)
        Right((i, offset + 1))
      else
        decodeIntRecursive(i, bytes, offset + 1, m + 7)
    }

  /** See RFC 7541 section 5.2. */
  private[codec] def decodeString(
    ctx: ImmutableDecoderContext
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
            ctx.bytes(ctx.offset),
            Some(err)
          )
        )
        case Left(err) => Left(err)
    }
  }

  private[codec] def decodeHuffman(
    ctx: ImmutableDecoderContext
  ): Either[HpackError, Boolean] =
    ctx.bytes.lift(ctx.offset) match {
      case Some(h) => Right((h & 0x80) != 0x00)
      case None => Left(HpackError.IncompleteInput(ctx.offset))
    }

  private def readString(
    ctx: ImmutableDecoderContext,
    h: Boolean,
    strOffset: Int,
    strSize: Int
  ): Chunk[Byte] = {
    val raw = ctx.bytes.slice(strOffset, strOffset + strSize)
    val chunk = Chunk.fromIterable(raw)
    if (h)
      HuffmanCodec.default.decode(chunk)
    else
      chunk
  }
}