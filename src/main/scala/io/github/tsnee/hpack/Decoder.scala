package io.github.tsnee.hpack

import scala.annotation.tailrec
import zio.Chunk

/** See RFC 7541 Section 3. */
trait Decoder {
  def decode(
    chunk: Chunk[Byte],
    ctx: DecoderContext
  ): DecoderContext
}

object Decoder {
  val default: Decoder = ChunkDecoder
}
