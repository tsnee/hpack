package io.github.tsnee.hpack

import zio.Chunk

case class HeaderField(
  name: Chunk[Byte],
  value: Chunk[Byte]
) {
  lazy val size: Int = name.size + value.size + HeaderField.overhead
  override lazy val toString: String =
    new String(name.toArray) + ": " + new String(value.toArray)
}

object HeaderField {
  val overhead: Int = 32

  def apply(
    name: String,
    value: String
  ): HeaderField =
    new HeaderField(name, value)
}
