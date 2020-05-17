package io.github.tsnee.hpack

import scala.annotation.tailrec
import zio.Chunk

/** See RFC 7541 Section 3. */
trait Decoder {
  def decode(
    chunk: Chunk[Byte],
    ctx: DecoderContext
  ): DecoderContext
}

object Decoder {
  val default: Decoder = VectorDecoder
}

private object VectorDecoder extends Decoder {
  override def decode(
    chunk: Chunk[Byte],
    ctx: DecoderContext
  ): DecoderContext = ctx match {
    case vectorCtx: VectorDecoderContext =>
      decodeRecursive(vectorCtx.copy(bytes = vectorCtx.bytes ++ chunk.toVector))
    case _ => ErrorDecoderContext(
      "This Decoder implementation does not work with this type of DecoderContext."
    )
  }

  @tailrec
  private def decodeRecursive(
    ctx: VectorDecoderContext
  ): VectorDecoderContext = {
    val headOpt = ctx.bytes.lift(ctx.offset)
    val newCtxOpt =
      if (headOpt.isEmpty)
        None
      else if ((headOpt.get & 0x80) != 0x00)
        indexedHeader(
          ctx,
          0x7F
        )
      else if (headOpt.get == 0x40)
        literalHeaderNewName(
          ctx.copy(offset = ctx.offset + 1),
          0x7F,
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
          0x7F,
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
          0x7F,
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
      case Some(newCtx) if newCtx.error.isEmpty => decodeRecursive(newCtx)
      case Some(newCtx) => newCtx   // Parse error
      case None => ctx              // Not enough input to parse one header
    }
  }

  /** See RFC 7541 section 6.1. */
  private def indexedHeader(
    ctx: VectorDecoderContext,
    mask: Byte
  ): Option[VectorDecoderContext] =
    decodeInt(mask, ctx.bytes, ctx.offset) match {
      case Right((idx, afterSize)) =>
        ctx.table.lookup(idx) match {
          case Some(headerField) => Some(
            ctx.copy(
              headers = headerField :: ctx.headers,
              bytes = ctx.bytes.drop(afterSize),
              offset = 0
            )
          )
          case None =>
            val maxIdx = StaticTable.numEntries + ctx.table.numEntries
            Some(
              ctx.copy(
                error = Some(
                  Error.InvalidInput(
                    s"Invalid header index $idx is not between 1 and $maxIdx.",
                    afterSize,
                    Expectation.HeaderIndex,
                    ctx.bytes(ctx.offset)
                  )
                )
              )
            )
        }
      case Left(err: Error.InvalidInput) => Some(
        ctx.copy(
          error = Some(
            Error.InvalidInput(
              "Cannot decode header index",
              ctx.offset,
              Expectation.HeaderIndex,
              ctx.bytes(ctx.offset),
              Some(err)
            )
          )
        )
      )
      case Left(err: Error.Implementation) => Some(ctx.copy(error = Some(err)))
      case Left(_: Error.IncompleteInput) => None
    }

  private def literalHeaderNewName(
    ctx: VectorDecoderContext,
    mask: Byte,
    indexing: Indexing
  ): Option[VectorDecoderContext] =
    decodeInt(mask, ctx.bytes, ctx.offset) match {
      case Right((nameLength, afterNameLength)) =>
        Console.err.println(s"nameLength $nameLength")
        decodeString(nameLength, ctx.bytes, afterNameLength) match {
          case Right((name, afterName)) =>
            Console.err.println(s"name ${new String(name.toArray)}")
            decodeInt(0x7F, ctx.bytes, afterName) match {
              case Right((valueLength, afterValueLength)) =>
                Console.err.println(s"valueLength $valueLength")
                decodeString(valueLength, ctx.bytes, afterValueLength) match {
                  case Right((value, afterValue)) =>
                    Console.err.println(s"value ${new String(value.toArray)}")
                    val headerField = HeaderField(name, value, indexing)
                    Some(
                      ctx.copy(
                        headers = headerField :: ctx.headers,
                        bytes = ctx.bytes.drop(afterValue),
                        offset = 0
                      )
                    )
                  case Left(err: Error.InvalidInput) => Console.err.println(s"value error $err");Some(
                    ctx.copy(
                      error = Some(
                        Error.InvalidInput(
                          "Cannot decode header value starting with " +
                            ctx.bytes(afterValueLength) + ".",
                          afterValueLength,
                          Expectation.HeaderValue,
                          ctx.bytes(afterValueLength),
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
              case Left(err: Error.InvalidInput) => Console.err.println(s"valueLength error $err");Some(
                ctx.copy(
                  error = Some(
                    Error.InvalidInput(
                      "Cannot decode header value length starting with " +
                        ctx.bytes(afterName) + ".",
                      afterName,
                      Expectation.NonZeroLength,
                      ctx.bytes(afterName),
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
          case Left(err: Error.InvalidInput) => Console.err.println(s"name error $err");Some(
            ctx.copy(
              error = Some(
                Error.InvalidInput(
                  "Cannot decode header name starting with " +
                    ctx.bytes(afterNameLength) + ".",
                  afterNameLength,
                  Expectation.HeaderName,
                  ctx.bytes(afterNameLength),
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
      case Left(err: Error.InvalidInput) => Console.err.println(s"name length error $err");Some(
        ctx.copy(
          error = Some(
            Error.InvalidInput(
              "Cannot decode header name length parameter starting with " +
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

  private def literalHeaderIndexedName(
    ctx: VectorDecoderContext,
    mask: Byte,
    indexing: Indexing
  ): Option[VectorDecoderContext] = {
    ???
  }

  private def resizeTable(
    ctx: VectorDecoderContext,
    mask: Byte
  ): Option[VectorDecoderContext] =
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
    bytes: Vector[Byte],
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
    bytes: Vector[Byte],
    offset: Int,
    m: Int
  ): Either[Error, (Int, Int)] =
    if (!bytes.isDefinedAt(offset)) {
      Console.err.println(s"decodeIntRecursive($value, $bytes, $offset, $m)")
      Left(Error.IncompleteInput(offset))
    }
    else if (m > 21)
      Left(Error.Implementation("Cannot handle a header size this big."))
    else {
      val b = bytes(offset)
      val i = value + (b & 0x7F) * Math.pow(2, m).toInt
      if ((b & 0x80) == 0x00)
        Right((i, offset + 1))
      else
        decodeIntRecursive(i, bytes, offset + 1, m + 7)
    }

  /** See RFC 7541 section 5.2. */
  private[hpack] def decodeString(
    length: Int,
    bytes: Vector[Byte],
    offset: Int
  ): Either[Error, (Chunk[Byte], Int)] = {
    Console.err.println(s"decodeString($length, $bytes, $offset)")
    if (offset + length <= bytes.size)
      Right((Chunk.fromIterable(bytes.drop(offset).take(length)), offset + length))
    else
      Left(Error.IncompleteInput(offset))
  }
}
