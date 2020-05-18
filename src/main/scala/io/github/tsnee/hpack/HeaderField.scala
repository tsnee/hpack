package io.github.tsnee.hpack

import scala.io.Codec
import zio.Chunk

case class HeaderField(
  name: Chunk[Byte],
  value: Chunk[Byte]
) {
  val overhead: Int = 32
  lazy val size: Int = name.size + value.size + overhead
  override lazy val toString: String =
    new String(name.toArray) + ": " + new String(value.toArray)
}

object HeaderField {
  def apply(
    name: String,
    value: String
  ): HeaderField =
    new HeaderField(fromString(name), fromString(value))

  private def fromString(s: String): Chunk[Byte] =
    Chunk.fromArray(s.getBytes(Codec.UTF8.charSet))
}
