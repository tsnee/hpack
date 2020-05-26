package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack.HeaderField
import zio.Chunk

trait Encoder {
  def encode(
    headerList: Seq[HeaderField],
    ctx: EncoderContext
  ): EncoderContext
}
