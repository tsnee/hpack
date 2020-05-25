package io.github.tsnee.hpack.codec.optimized

import io.github.tsnee.hpack._
import io.github.tsnee.hpack.codec.{Decoder, DecoderContext}
import io.github.tsnee.hpack.huffman.HuffmanCodec
import io.github.tsnee.hpack.table.{DynamicTable, Indexing, StaticTable}
import zio.Chunk

import scala.annotation.tailrec

private[codec] class MutableDecoderContext(
  var table: DynamicTable,
  var bytes: Chunk[Byte] = Chunk.empty,
  var offset: Int = 0,
  var headers: List[HeaderField] = Nil,
  var error: Option[HpackError] = None
) extends DecoderContext {
  override def headerList: (Seq[HeaderField], DecoderContext) = {
    assert(bytes.size == offset)  // all input consumed
    (headers.reverse, new MutableDecoderContext(table))
  }

  override def toString: String =
    s"MutableDecoderContext($table, ${bytes.map(_.toHexString)}, $offset, $headers, $error)"
}

private[codec] object MutableDecoder extends Decoder {
  override def decode(
    chunk: Chunk[Byte],
    ctx: DecoderContext
  ): Either[HpackError, DecoderContext] =
    if (ctx.isInstanceOf[MutableDecoderContext]) {
      val chunkCtx = ctx.asInstanceOf[MutableDecoderContext]
      chunkCtx.bytes = chunkCtx.bytes ++ chunk
      decodeRecursive(chunkCtx)
      if (chunkCtx.error.isEmpty)
        Right(chunkCtx)
      else
        Left(chunkCtx.error.get)
    }
    else
      Left(
        HpackError.Implementation(
          "This Decoder implementation does not work with this type of DecoderContext."
        )
      )

  @tailrec
  private def decodeRecursive(
    ctx: MutableDecoderContext
  ): Unit =
    if (ctx.bytes.isDefinedAt(ctx.offset)) {
      //Console.err.println(s"decodeRecursive($ctx)")
      val origTable = ctx.table
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
        resizeTable(ctx)
      else {
        val error = HpackError.InvalidInput(
          "Could not parse header block.",
          origOffset,
          HpackError.Expectation.FirstHeaderByte,
          head
        )
        ctx.error = Some(error)
      }
      //Console.err.println(s"Comparing new ${ctx.headers} to old $origHeaders")
      if (ctx.headers != origHeaders || ctx.table != origTable)
        decodeRecursive(ctx)
      else if (ctx.error.isEmpty)   // not enough input
        ctx.offset = origOffset     // start over again when we get more
    }

  /** See RFC 7541 section 6.1. */
  private def indexedHeader(
    ctx: MutableDecoderContext
  ): Unit = {
    val either = decodeInt(0x7F, ctx.bytes, ctx.offset)
    if (either.isRight) {
      val tuple = either.getOrElse((-1, -1))
      val idx = tuple._1
      val afterIdx = tuple._2
      val option = ctx.table.lookup(idx)
      if (option.nonEmpty) {
        val headerField = option.get
        ctx.headers = headerField :: ctx.headers
        ctx.offset = afterIdx
      }
      else
        tableLookupFailure(ctx, idx)
    }
    else {
      val error = either.left.getOrElse(HpackError.Implementation("Programmer error"))
      if (error.isInstanceOf[HpackError.InvalidInput])
        decodeHeaderIndexFailure(ctx, error.asInstanceOf[HpackError.InvalidInput])
      else if (error.isInstanceOf[HpackError.Implementation])
        ctx.error = Some(error)
      // else end of input
    }
  }

  private def decodeHeaderIndexFailure(
                                        ctx: MutableDecoderContext,
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
                                    ctx: MutableDecoderContext,
                                    indexing: Indexing
  ): Unit = {
    val either = decodeString(ctx)
    if (either.isRight) {
      val tuple = either.getOrElse((Chunk.empty, -1))
      val name = tuple._1
      val afterName = tuple._2
      //Console.err.println(s"name ${new String(name.toArray)}")
      ctx.offset = afterName
      decodeValue(name, ctx, indexing)
    }
    else {
      val error = either
        .left
        .getOrElse(HpackError.Implementation("Programmer error"))
      //Console.err.println(s"name error $error")
      if (error.isInstanceOf[HpackError.InvalidInput])
        ctx.error = Some(
          HpackError.InvalidInput(
            "Cannot decode header name starting with " +
              ctx.bytes.byte(ctx.offset) + ".",
            ctx.offset,
            HpackError.Expectation.HeaderName,
            ctx.bytes.byte(ctx.offset),
            Some(error)
          )
        )
      else if (error.isInstanceOf[HpackError.Implementation])
        ctx.error = Some(error)
      // else end of input
    }
  }

  private def decodeValue(
                           name: Chunk[Byte],
                           ctx: MutableDecoderContext,
                           indexing: Indexing
  ): Unit = {
    val either = decodeString(ctx)
    if (either.isRight) {
      val tuple = either.getOrElse((Chunk.empty, -1))
      val value = tuple._1
      val afterValue = tuple._2
      //Console.err.println(s"name ${new String(name.toArray)} value ${new String(value.toArray)} indexing $indexing")
      val headerField = HeaderField(name, value)
      ctx.headers = headerField :: ctx.headers
      ctx.offset = afterValue
      ctx.table = ctx.table.store(headerField, indexing)
    }
    else {
      val error = either
        .left
        .getOrElse(HpackError.Implementation("Programmer error"))
      if (error.isInstanceOf[HpackError.InvalidInput])
        ctx.error = Some(
          HpackError.InvalidInput(
            "Cannot decode header value starting with " +
              ctx.bytes.byte(ctx.offset) + ".",
            ctx.offset,
            HpackError.Expectation.HeaderValue,
            ctx.bytes.byte(ctx.offset),
            Some(error)
          )
        )
      else if (error.isInstanceOf[HpackError.Implementation])
        ctx.error = Some(error)
      // else end of input
    }
  }

  private def literalHeaderIndexedName(
                                        ctx: MutableDecoderContext,
                                        mask: Byte,
                                        indexing: Indexing
  ): Unit = {
    val either = decodeInt(mask, ctx.bytes, ctx.offset)
    if (either.isRight) {
      val tuple = either.getOrElse((-1, -1))
      val idx = tuple._1
      val afterIdx = tuple._2
      val option = ctx.table.lookup(idx)
      if (option.nonEmpty) {
        val name = option.get.name
        //Console.err.println(s"Looked up ${new String(name.toArray)}")
        ctx.offset = afterIdx
        decodeValue(name, ctx, indexing)
      }
      else
        tableLookupFailure(ctx, idx)
    }
    else {
      val error = either
        .left
        .getOrElse(HpackError.Implementation("Programmer error"))
      if (error.isInstanceOf[HpackError.InvalidInput])
        decodeHeaderIndexFailure(ctx, error)
      else if (error.isInstanceOf[HpackError.Implementation])
        ctx.error = Some(error)
      // else end of input
    }
  }

  private def tableLookupFailure(
                                  ctx: MutableDecoderContext,
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
    ctx: MutableDecoderContext
  ): Unit = {
    val either = decodeInt(0x1F, ctx.bytes, ctx.offset)
    if (either.isRight) {
      val tuple = either.getOrElse((-1, -1))
      val newSize = tuple._1
      val newOffset = tuple._2
      ctx.table = ctx.table.resize(newSize)
      ctx.offset = newOffset
    }
    else {
      val error = either
        .left
        .getOrElse(HpackError.Implementation("Programmer error"))
      if (error.isInstanceOf[HpackError.InvalidInput])
        ctx.error = Some(
          HpackError.InvalidInput(
            "Cannot decode table size parameter starting with " +
              ctx.bytes.byte(ctx.offset) + ".",
            ctx.offset,
            HpackError.Expectation.NonZeroLength,
            ctx.bytes.byte(ctx.offset),
            Some(error)
          )
        )
      else if (error.isInstanceOf[HpackError.Implementation])
        ctx.error = Some(error)
      // else end of input
    }
  }

  /** See RFC 7541 section 5.1. */
  private[codec] def decodeInt(
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
  private[codec] def decodeString(
    ctx: MutableDecoderContext
  ): Either[HpackError, (Chunk[Byte], Int)] =
    decodeHuffman(ctx).flatMap { h =>
      val either = decodeInt(0x7F, ctx.bytes, ctx.offset)
      if (either.isRight) {
        val tuple = either.getOrElse((-1, -1))
        val strSize = tuple._1
        val strOffset = tuple._2
        if (strOffset + strSize > ctx.bytes.size)
          Left(HpackError.IncompleteInput(strOffset))
        else
          Right((readString(ctx, h, strOffset, strSize), strOffset + strSize))
      }
      else {
        val error = either
          .left
          .getOrElse(HpackError.Implementation("Programmer error"))
        if (error.isInstanceOf[HpackError.InvalidInput])
          Left(
            HpackError.InvalidInput(
              "Cannot decode string length.",
              ctx.offset,
              HpackError.Expectation.NonZeroLength,
              ctx.bytes.byte(ctx.offset),
              Some(error)
            )
          )
        else
          Left(error)
    }
  }

  private[codec] def decodeHuffman(
    ctx: MutableDecoderContext
  ): Either[HpackError, Boolean] =
    if (ctx.bytes.isDefinedAt(ctx.offset))
      Right((ctx.bytes.byte(ctx.offset) & 0x80) != 0x00)
    else
      Left(HpackError.IncompleteInput(ctx.offset))

  private def readString(
                          ctx: MutableDecoderContext,
                          h: Boolean,
                          strOffset: Int,
                          strSize: Int
  ): Chunk[Byte] = {
    val str = ctx.bytes.slice(strOffset, strOffset + strSize)
    if (h)
      HuffmanCodec.default.decode(str)
    else
      str
  }
}
