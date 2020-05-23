package io.github.tsnee.hpack

import zio.Chunk

trait HuffmanCodec {
  def encode(input: IndexedSeq[Byte]): Chunk[Byte]
  def decode(input: IndexedSeq[Byte]): Chunk[Byte]
}

object HuffmanCodec {
  val default: HuffmanCodec = HuffmanTree
}
