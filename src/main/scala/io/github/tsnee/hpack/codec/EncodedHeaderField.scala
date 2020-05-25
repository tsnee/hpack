package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack._
import zio.Chunk

sealed trait EncodedHeaderField {
  def representation: Chunk[Byte]
}

class IndexedHeaderField(
  encoder: Encoder,
  idx: Int
) extends EncodedHeaderField {
  override lazy val representation: Chunk[Byte] =
    encoder.encodePositiveInt(0x80, 1, idx)
}
