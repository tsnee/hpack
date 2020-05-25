package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack.HeaderField
import zio.Chunk

trait Encoder {
  def encode(
    headerList: Seq[HeaderField],
    ctx: EncoderContext
  ): (Chunk[Byte], EncoderContext)

  private[codec] def encodePositiveInt(
    prefix: Byte,
    prefixLen: Int,
    i: Int
  ): Chunk[Byte]
}
