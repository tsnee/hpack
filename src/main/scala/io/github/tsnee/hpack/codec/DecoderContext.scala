package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack.{HpackError, HeaderField}
import io.github.tsnee.hpack.table.DynamicTable
import zio.Chunk

/** See RFC 7541 Section 2.2. */
trait DecoderContext {
  val headerList: Seq[HeaderField]
}

object DecoderContext {
  def default(dynamicTableSize: Int): DecoderContext =
    VectorDecoderContext(DynamicTable(dynamicTableSize))
}

private[codec] class ErrorDecoderContext(
  err: HpackError
) extends DecoderContext {
  override val headerList: Seq[HeaderField] = Seq.empty
  val error = Some(err)
}

object ErrorDecoderContext {
  def apply(message: String): ErrorDecoderContext =
    new ErrorDecoderContext(HpackError.Implementation(message))

  def apply(
             message: String,
             location: Int,
             expected: HpackError.Expectation,
             actual: Byte
  ): ErrorDecoderContext =
    new ErrorDecoderContext(
      HpackError.InvalidInput(message, location, expected, actual)
    )
}

private[codec] class ChunkDecoderContext(
  var table: DynamicTable,
  var bytes: Chunk[Byte] = Chunk.empty,
  var offset: Int = 0,
  var headers: List[HeaderField] = Nil,
  var error: Option[HpackError] = None
) extends DecoderContext {
  override lazy val headerList: Seq[HeaderField] = headers.reverse
}

private[codec] case class VectorDecoderContext(
  table: DynamicTable,
  bytes: Vector[Byte] = Vector.empty,
  offset: Int = 0,
  headers: List[HeaderField] = Nil,
  error: Option[HpackError] = None
) extends DecoderContext {
  override lazy val headerList: Seq[HeaderField] = headers.reverse
}
