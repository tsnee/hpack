package io.github.tsnee.hpack

sealed trait HpackError

object HpackError {
  sealed trait Expectation
  object Expectation {
    case object FirstHeaderByte extends Expectation
    case object HeaderIndex extends Expectation
    case object NonZeroLength extends Expectation
    case object HeaderName extends Expectation
    case object HeaderValue extends Expectation
  }

  case class Implementation(message: String) extends HpackError

  case class InvalidInput(
    message: String,
    location: Int,
    expected: Expectation,
    actual: Byte,
    cause: Option[HpackError] = None
  ) extends HpackError

  case class IncompleteInput(location: Int) extends HpackError
}
