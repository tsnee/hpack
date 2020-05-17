package io.github.tsnee.hpack

import scala.io.Codec
import zio.Chunk

sealed trait Indexing
object Indexing {
  case object With extends Indexing
  case object Without extends Indexing
  case object Never extends Indexing
}

case class HeaderField(
  name: Chunk[Byte],
  value: Chunk[Byte],
  indexing: Indexing
) {
  val overhead: Int = 32
  lazy val size: Int = name.size + value.size + overhead
}

object HeaderField {
  def apply(
    name: String,
    value: String,
    indexing: Indexing = Indexing.With
  ): HeaderField =
    new HeaderField(fromString(name), fromString(value), indexing)

  private def fromString(s: String): Chunk[Byte] =
    Chunk.fromArray(s.getBytes(Codec.UTF8.charSet))
}
