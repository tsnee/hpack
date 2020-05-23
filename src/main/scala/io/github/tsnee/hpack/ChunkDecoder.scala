package io.github.tsnee.hpack

import scala.annotation.tailrec
import zio.Chunk

private object ChunkDecoder extends Decoder {
  override def decode(
    chunk: Chunk[Byte],
    ctx: DecoderContext
  ): DecoderContext = ctx match {
    case chunkCtx: ChunkDecoderContext =>
      decodeRecursive(chunkCtx.copy(bytes = chunkCtx.bytes ++ chunk))
    case _ => ErrorDecoderContext(
      "This Decoder implementation does not work with this type of DecoderContext."
    )
  }

  @tailrec
  private def decodeRecursive(
    ctx: ChunkDecoderContext
  ): ChunkDecoderContext = {
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
        resizeTable(ctx, 0x1F)
      else {
        val error = Error.InvalidInput(
          "Could not parse header block.",
          ctx.offset,
          Expectation.FirstHeaderByte,
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
    ctx: ChunkDecoderContext
  ): Option[ChunkDecoderContext] =
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
      case Left(err: Error.InvalidInput) => decodeHeaderIndexFailure(ctx, err)
      case Left(err: Error.Implementation) => Some(ctx.copy(error = Some(err)))
      case Left(_: Error.IncompleteInput) => None
    }

  private def decodeHeaderIndexFailure(
    ctx: ChunkDecoderContext,
    chained: Error
  ): Option[ChunkDecoderContext] =
    Some(
      ctx.copy(
        error = Some(
          Error.InvalidInput(
            "Cannot decode header index",
            ctx.offset,
            Expectation.HeaderIndex,
            ctx.bytes(ctx.offset),
            Some(chained)
          )
        )
      )
    )

  private def literalHeaderNewName(
    ctx: ChunkDecoderContext,
    indexing: Indexing
  ): Option[ChunkDecoderContext] =
    decodeString(ctx) match {
      case Right((name, afterName)) =>
        //Console.err.println(s"name ${new String(name.toArray)}")
        decodeValue(name, ctx.copy(offset = afterName), indexing)
      case Left(err: Error.InvalidInput) => /* Console.err.println(s"name error $err"); */ Some(
        ctx.copy(
          error = Some(
            Error.InvalidInput(
              "Cannot decode header name starting with " +
                ctx.bytes(ctx.offset) + ".",
              ctx.offset,
              Expectation.HeaderName,
              ctx.bytes(ctx.offset),
              Some(err)
            )
          )
        )
      )
      case Left(err: Error.Implementation) => Some(ctx.copy(error = Some(err)))
      case Left(_: Error.IncompleteInput) => None
  }

  private def decodeValue(
    name: Chunk[Byte],
    ctx: ChunkDecoderContext,
    indexing: Indexing
  ): Option[ChunkDecoderContext] =
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
      case Left(err: Error.InvalidInput) => /* Console.err.println(s"value error $err"); */ Some(
        ctx.copy(
          error = Some(
            Error.InvalidInput(
              "Cannot decode header value starting with " +
                ctx.bytes(ctx.offset) + ".",
              ctx.offset,
              Expectation.HeaderValue,
              ctx.bytes(ctx.offset),
              Some(err)
            )
          )
        )
      )
      case Left(err: Error.Implementation) => Some(
        ctx.copy(
          error = Some(err)
        )
      )
      case Left(_: Error.IncompleteInput) => None
    }

  private def literalHeaderIndexedName(
    ctx: ChunkDecoderContext,
    mask: Byte,
    indexing: Indexing
  ): Option[ChunkDecoderContext] =
    decodeInt(mask, ctx.bytes, ctx.offset) match {
      case Right((idx, afterIdx)) =>
        ctx.table.lookup(idx) match {
          case Some(HeaderField(name, _)) =>
            //Console.err.println(s"Looked up ${new String(name.toArray)}")
            decodeValue(name, ctx.copy(offset = afterIdx), indexing)
          case None => tableLookupFailure(ctx, idx)
        }
      case Left(err: Error.InvalidInput) => decodeHeaderIndexFailure(ctx, err)
      case Left(err: Error.Implementation) => Some(ctx.copy(error = Some(err)))
      case Left(_: Error.IncompleteInput) => None
    }

  private def tableLookupFailure(
    ctx: ChunkDecoderContext,
    idx: Int
  ): Option[ChunkDecoderContext] = {
    val maxIdx = StaticTable.numEntries + ctx.table.numEntries
    Some(
      ctx.copy(
        error = Some(
          Error.InvalidInput(
            s"Invalid header index $idx is not between 1 and $maxIdx.",
            ctx.offset,
            Expectation.HeaderIndex,
            ctx.bytes(ctx.offset)
          )
        )
      )
    )
  }

  private def resizeTable(
    ctx: ChunkDecoderContext,
    mask: Byte
  ): Option[ChunkDecoderContext] =
    decodeInt(mask, ctx.bytes, ctx.offset) match {
      case Right((newSize, newOffset)) => Some(
        ctx.copy(
          table = ctx.table.resize(newSize),
          bytes = ctx.bytes.drop(newOffset),
          offset = 0
        )
      )
      case Left(err: Error.InvalidInput) => Some(
        ctx.copy(
          error = Some(
            Error.InvalidInput(
              "Cannot decode table size parameter starting with " +
                ctx.bytes(ctx.offset) + ".",
              ctx.offset,
              Expectation.NonZeroLength,
              ctx.bytes(ctx.offset),
              Some(err)
            )
          )
        )
      )
      case Left(err: Error.Implementation) => Some(ctx.copy(error = Some(err)))
      case Left(_: Error.IncompleteInput) => None
    }

  /** See RFC 7541 section 5.1. */
  private[hpack] def decodeInt(
    mask: Byte,
    bytes: Chunk[Byte],
    offset: Int
  ): Either[Error, (Int, Int)] =
    if (bytes.isDefinedAt(offset)) {
      val value = bytes(offset) & mask
      if (value != mask)  // i.e. not all 1s
        Right((value, offset + 1))
      else
        decodeIntRecursive(value, bytes, offset + 1, 0)
    }
    else
      Left(Error.IncompleteInput(offset))

  @tailrec
  private def decodeIntRecursive(
    value: Int,
    bytes: Chunk[Byte],
    offset: Int,
    m: Int
  ): Either[Error, (Int, Int)] =
    if (!bytes.isDefinedAt(offset)) {
      //Console.err.println(s"decodeIntRecursive($value, $bytes, $offset, $m)")
      Left(Error.IncompleteInput(offset))
    }
    else if (m > 21)
      Left(Error.Implementation("Cannot handle a header size this big."))
    else {
      val b = bytes(offset)
      val i = value + ((b & 0x7F) << m)
      if ((b & 0x80) == 0x00)
        Right((i, offset + 1))
      else
        decodeIntRecursive(i, bytes, offset + 1, m + 7)
    }

  /** See RFC 7541 section 5.2. */
  private[hpack] def decodeString(
    ctx: ChunkDecoderContext
  ): Either[Error, (Chunk[Byte], Int)] =
    decodeHuffman(ctx).flatMap { h =>
      decodeInt(0x7F, ctx.bytes, ctx.offset) match {
        case Right((strSize, strOffset)) =>
          if (strOffset + strSize > ctx.bytes.size)
            Left(Error.IncompleteInput(strOffset))
          else
            Right((readString(ctx, h, strOffset, strSize), strOffset + strSize))
        case Left(err: Error.InvalidInput) => Left(
          Error.InvalidInput(
            "Cannot decode string length.",
            ctx.offset,
            Expectation.NonZeroLength,
            ctx.bytes(ctx.offset),
            Some(err)
          )
        )
        case Left(err) => Left(err)
    }
  }

  private[hpack] def decodeHuffman(
    ctx: ChunkDecoderContext
  ): Either[Error, Boolean] =
    ctx.bytes.lift(ctx.offset) match {
      case Some(h) => Right((h & 0x80) != 0x00)
      case None => Left(Error.IncompleteInput(ctx.offset))
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
