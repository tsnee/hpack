package io.github.tsnee.hpack.codec

import zio.Chunk

trait EncoderBenchmark {
  def rfc7541AppendixC_1_1: Chunk[Byte]
  def rfc7541AppendixC_1_2: Chunk[Byte]
  def rfc7541AppendixC_1_3: Chunk[Byte]
}
