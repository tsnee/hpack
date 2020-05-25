package io.github.tsnee.hpack.huffman

import zio.Chunk

trait HuffmanCodec {
  def encode(input: Chunk[Byte]): Chunk[Byte]
  def decode(input: Chunk[Byte]): Chunk[Byte]
}

object HuffmanCodec {
  val default: HuffmanCodec = HuffmanTree
}
