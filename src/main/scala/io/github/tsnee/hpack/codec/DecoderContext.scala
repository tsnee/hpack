package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack.{HpackError, HeaderField}
import io.github.tsnee.hpack.table.DynamicTable
import io.github.tsnee.hpack.codec.functional.ImmutableDecoderContext

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
