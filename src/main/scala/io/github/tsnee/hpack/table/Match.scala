package io.github.tsnee.hpack.table

sealed trait Match

object Match {
  case class Full(idx: Int) extends Match
  case class Partial(idx: Int) extends Match
  case object NotFound extends Match
}