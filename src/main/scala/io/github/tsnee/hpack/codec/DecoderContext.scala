package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack.{HpackError, HeaderField}
import io.github.tsnee.hpack.table.DynamicTable
import zio.Chunk

/** See RFC 7541 Section 2.2. */
trait DecoderContext {
  /** Get the complete list of headers and a new DecoderContext.
    * The new context will have no headers or accumulated input,
    * but will contain the same dynamic table.
    */
  def headerList: (Seq[HeaderField], DecoderContext)
}

object DecoderContext {
  def default(dynamicTableSize: Int): DecoderContext =
    ImmutableDecoderContext(DynamicTable(dynamicTableSize))
}

private[codec] class ErrorDecoderContext(
  err: HpackError
) extends DecoderContext {
  override def headerList: (Seq[HeaderField], DecoderContext) =
    (Seq.empty, this)
  val error: Option[HpackError] = Some(err)
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

private[codec] class MutableDecoderContext(
  var table: DynamicTable,
  var bytes: Chunk[Byte] = Chunk.empty,
  var offset: Int = 0,
  var headers: List[HeaderField] = Nil,
  var error: Option[HpackError] = None
) extends DecoderContext {
  override def headerList: (Seq[HeaderField], DecoderContext) = {
    assert (bytes.size == offset)  // all input consumed
    (headers.reverse, new MutableDecoderContext(table))
  }

  override def toString: String =
    s"MutableDecoderContext($table, ${bytes.map(_.toHexString)}, $offset, $headers, $error)"
}

private[codec] case class ImmutableDecoderContext(
  table: DynamicTable,
  bytes: Vector[Byte] = Vector.empty,
  offset: Int = 0,
  headers: List[HeaderField] = Nil,
  error: Option[HpackError] = None
) extends DecoderContext {
  override def headerList: (Seq[HeaderField], DecoderContext) = {
    assert (bytes.size == offset)  // all input consumed
    (headers.reverse, ImmutableDecoderContext(table))
  }
}
