package io.github.tsnee.hpack

import zio.Chunk

/** See RFC 7541 Section 2.2. */
trait DecoderContext {
  val headerList: Seq[HeaderField]
  val error: Option[Error]
  def flush: DecoderContext
}

object DecoderContext {
  def default(dynamicTableSize: Int): DecoderContext =
    VectorDecoderContext(DynamicTable(dynamicTableSize))
}

private[hpack] class ErrorDecoderContext(
  err: Error
) extends DecoderContext {
  override val headerList = Seq.empty
  override val error = Some(err)
  override lazy val flush: DecoderContext = this
}

object ErrorDecoderContext {
  def apply(message: String): ErrorDecoderContext =
    new ErrorDecoderContext(Error.Implementation(message))

  def apply(
    message: String,
    location: Int,
    expected: Expectation,
    actual: Byte
  ): ErrorDecoderContext =
    new ErrorDecoderContext(
      Error.InvalidInput(message, location, expected, actual)
    )
}

private[hpack] case class ChunkDecoderContext(
  table: DynamicTable,
  bytes: Chunk[Byte] = Chunk.empty,
  offset: Int = 0,
  headers: List[HeaderField] = Nil,
  error: Option[Error] = None
) extends DecoderContext {
  override lazy val headerList: Seq[HeaderField] = headers.reverse
  override lazy val flush: DecoderContext = ChunkDecoderContext(table)
}

private[hpack] case class VectorDecoderContext(
  table: DynamicTable,
  bytes: Vector[Byte] = Vector.empty,
  offset: Int = 0,
  headers: List[HeaderField] = Nil,
  error: Option[Error] = None
) extends DecoderContext {
  override lazy val headerList: Seq[HeaderField] = headers.reverse
  override lazy val flush: DecoderContext = VectorDecoderContext(table)
}
