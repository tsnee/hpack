package io.github.tsnee

import scala.io.Codec
import scala.language.implicitConversions
import zio.Chunk

package object hpack {
  implicit def stringToByteChunk(s: String) =
    Chunk.fromArray(s.getBytes(Codec.UTF8.charSet))

  implicit def intToByte(i: Int): Byte = i.toByte
}
