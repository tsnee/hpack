package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack.HpackError
import zio.Chunk

/** See RFC 7541 Section 3. */
trait Decoder {
  def decode(
    chunk: Chunk[Byte],
    ctx: DecoderContext
  ): Either[HpackError, DecoderContext]
}

object Decoder {
  val default: Decoder = MutableDecoder
}
