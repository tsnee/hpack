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
      decodeRecursive(vectorCtx, chunk)
    case _ => ErrorDecoderContext(
      "This Decoder implementation does not work with this type of DecoderContext."
    )
  }

  @tailrec
  private def decodeRecursive(
    ctx: VectorDecoderContext,
    input: Chunk[Byte] = Chunk.empty
  ): VectorDecoderContext = ctx.error match {
    case Some(_: Error.IncompleteInput) if input.isEmpty => ctx
    case _ =>
      val newBytes = ctx.bytes ++ input.toVector
      val firstByte = newBytes.headOption
      if (firstByte.isEmpty)
        ctx.copy(bytes = newBytes)
      else if ((firstByte.get & 0x80) != 0x00)
        decodeRecursive(
          indexedHeader(
            ctx,
            0x7F,
            newBytes,
            0
          )
        )
      else if (firstByte.get == 0x40)
        decodeRecursive(
          literalHeaderNewName(
            ctx,
            0x7F,
            newBytes,
            1,
            Indexing.With
          )
        )
      else if ((firstByte.get & 0xC0) == 0x40)
        decodeRecursive(
          literalHeaderIndexedName(
            ctx,
            0x3F,
            newBytes,
            0,
            Indexing.With
          )
        )
      else if (firstByte.get == 0x00)
        decodeRecursive(
          literalHeaderNewName(
            ctx,
            0x7F,
            newBytes,
            1,
            Indexing.Without
          )
        )
      else if ((firstByte.get & 0xF0) == 0x00)
        decodeRecursive(
          literalHeaderIndexedName(
            ctx,
            0x0F,
            newBytes,
            0,
            Indexing.Without
          )
        )
      else if (firstByte.get == 0x10)
        decodeRecursive(
          literalHeaderNewName(
            ctx,
            0x7F,
            newBytes,
            1,
            Indexing.Never
          )
        )
      else if ((firstByte.get & 0xF0) == 0x10)
        decodeRecursive(
          literalHeaderIndexedName(
            ctx,
            0x0F,
            newBytes,
            0,
            Indexing.Never
          )
        )
      else if ((firstByte.get & 0xE0) != 0x00)
        decodeRecursive(resizeTable(ctx, 0x1F, newBytes, 0))
      else {
        val error = Error.InvalidInput(
          "Could not parse header block.",
          0,
          Expectation.FirstHeaderByte,
          firstByte.get
        )
        ctx.copy(error = Some(error))
      }
  }

/** See section 6.1. */
  private def indexedHeader(
    ctx: VectorDecoderContext,
    mask: Byte,
    bytes: Vector[Byte],
    offset: Int
  ): VectorDecoderContext =
    decodeInt(mask, bytes, offset) match {
      case Right((idx, afterSize)) =>
        ctx.table.lookup(idx) match {
          case Some(headerField) => ctx.copy(
            headers = headerField :: ctx.headers,
            bytes = bytes.drop(afterSize)
          )
          case None =>
            val maxIdx = StaticTable.numEntries + ctx.table.numEntries
            ctx.copy(
              error = Some(
                Error.InvalidInput(
                  s"Invalid header index $idx is not between 1 and $maxIdx.",
                  afterSize,
                  Expectation.HeaderIndex,
                  bytes(offset)
                )
              )
            )
        }
      case Left(err: Error.InvalidInput) => ctx.copy(
        error = Some(
          Error.InvalidInput(
            "Cannot decode header index",
            offset,
            Expectation.HeaderIndex,
            bytes(offset),
            Some(err)
          )
        )
      )
      case Left(err) => ctx.copy(error = Some(err))
    }

  private def literalHeaderNewName(
    ctx: VectorDecoderContext,
    mask: Byte,
    bytes: Vector[Byte],
    offset: Int,
    indexing: Indexing
  ): VectorDecoderContext =
    decodeInt(mask, bytes, offset) match {
      case Right((nameLength, afterNameLength)) =>
        //Console.err.println(s"nameLength $nameLength")
        decodeString(nameLength, bytes, afterNameLength) match {
          case Right((name, afterName)) =>
            //Console.err.println(s"name ${new String(name.toArray)}")
            decodeInt(0x7F, bytes, afterName) match {
              case Right((valueLength, afterValueLength)) =>
                decodeString(valueLength, bytes, afterValueLength) match {
                  case Right((value, afterValue)) =>
                    //Console.err.println(s"valueLength $valueLength")
                    val headerField = HeaderField(name, value, indexing)
                    ctx.copy(
                      bytes = bytes.drop(afterValue),
                      headers = headerField :: ctx.headers
                    )
                  case Left(err: Error.InvalidInput) => ctx.copy(
                    bytes = bytes,
                    error = Some(
                      Error.InvalidInput(
                        "Cannot decode header value starting with " +
                          bytes(afterValueLength) + ".",
                        afterValueLength,
                        Expectation.HeaderValue,
                        bytes(afterValueLength),
                        Some(err)
                      )
                    )
                  )
                  case Left(err) => ctx.copy(
                    bytes = bytes,
                    error = Some(err)
                  )
                }
              case Left(err: Error.InvalidInput) => ctx.copy(
                bytes = bytes,
                error = Some(
                  Error.InvalidInput(
                    "Cannot decode header value length starting with " +
                      bytes(afterName) + ".",
                    afterName,
                    Expectation.NonZeroLength,
                    bytes(afterName),
                    Some(err)
                  )
                )
              )
              case Left(err) => ctx.copy(
                bytes = bytes,
                error = Some(err)
              )
            }
          case Left(err: Error.InvalidInput) => ctx.copy(
            bytes = bytes,
            error = Some(
              Error.InvalidInput(
                "Cannot decode header name name starting with " +
                  bytes(afterNameLength) + ".",
                afterNameLength,
                Expectation.HeaderName,
                bytes(afterNameLength),
                Some(err)
              )
            )
          )
          case Left(err) => ctx.copy(
            bytes = bytes,
            error = Some(err)
          )
        }
      case Left(err: Error.InvalidInput) => ctx.copy(
        bytes = bytes,
        error = Some(
          Error.InvalidInput(
            "Cannot decode header name length parameter starting with " +
              bytes(offset) + ".",
            offset,
            Expectation.NonZeroLength,
            bytes(offset),
            Some(err)
          )
        )
      )
      case Left(err) => ctx.copy(
        bytes = bytes,
        error = Some(err)
      )
  }

  private def literalHeaderIndexedName(
    ctx: VectorDecoderContext,
    mask: Byte,
    bytes: Vector[Byte],
    offset: Int,
    indexing: Indexing
  ): VectorDecoderContext = {
    ???
  }

  private def resizeTable(
    ctx: VectorDecoderContext,
    mask: Byte,
    bytes: Vector[Byte],
    offset: Int
  ): VectorDecoderContext =
    decodeInt(mask, bytes, offset) match {
      case Right((newSize, newOffset)) => ctx.copy(
        table = ctx.table.resize(newSize),
        bytes = bytes.drop(newOffset)
      )
      case Left(err: Error.InvalidInput) => ctx.copy(
        bytes = bytes,
        error = Some(
          Error.InvalidInput(
            s"Cannot decode table size parameter starting with ${bytes(offset)}.",
            offset,
            Expectation.NonZeroLength,
            bytes(offset),
            Some(err)
          )
        )
      )
      case Left(err) => ctx.copy(
        bytes = bytes,
        error = Some(err)
      )
    }

  /** See section 5.1. */
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
    if (!bytes.isDefinedAt(offset))
      Left(Error.IncompleteInput(offset))
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

  /** See section 5.2. */
  private[hpack] def decodeString(
    length: Int,
    bytes: Vector[Byte],
    offset: Int
  ): Either[Error, (Chunk[Byte], Int)] =
    if (offset + length < bytes.size)
      Right((Chunk.fromIterable(bytes.drop(offset).take(length)), offset + length))
    else
      Left(Error.IncompleteInput(offset))
}
