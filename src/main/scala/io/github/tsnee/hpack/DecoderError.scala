package io.github.tsnee.hpack

sealed trait DecoderError

object DecoderError {
  sealed trait Expectation
  object Expectation {
    case object FirstHeaderByte extends Expectation
    case object HeaderIndex extends Expectation
    case object NonZeroLength extends Expectation
    case object HeaderName extends Expectation
    case object HeaderValue extends Expectation
  }

  case class Implementation(message: String) extends DecoderError

  case class InvalidInput(
    message: String,
    location: Int,
    expected: Expectation,
    actual: Byte,
    cause: Option[DecoderError] = None
  ) extends DecoderError

  case class IncompleteInput(location: Int) extends DecoderError
}
