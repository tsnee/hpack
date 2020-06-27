package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack.HeaderField
import io.github.tsnee.hpack.codec.functional.ImmutableEncoder

trait Encoder {
  def encode(
    headerList: Seq[HeaderField],
    ctx: EncoderContext
  ): EncoderContext
}

object Encoder {
  def default: Encoder = ImmutableEncoder
}