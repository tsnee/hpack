package io.github.tsnee.hpack

sealed trait Expectation
object Expectation {
  case object FirstHeaderByte extends Expectation
  case object HeaderIndex extends Expectation
  case object NonZeroLength extends Expectation
  case object HeaderName extends Expectation
  case object HeaderValue extends Expectation
}

sealed trait Error

object Error {
  case class Implementation(message: String) extends Error

  case class InvalidInput(
    message: String,
    location: Int,
    expected: Expectation,
    actual: Byte,
    cause: Option[Error] = None
  ) extends Error

  case class IncompleteInput(location: Int) extends Error
}
