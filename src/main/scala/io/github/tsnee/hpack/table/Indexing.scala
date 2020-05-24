package io.github.tsnee.hpack.table

sealed trait Indexing
object Indexing {
  case object With extends Indexing
  case object Without extends Indexing
  case object Never extends Indexing
}
