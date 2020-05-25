package io.github.tsnee.hpack.codec.functional

import io.github.tsnee.hpack.codec.EncoderBenchmark
import io.github.tsnee.hpack.codec.Fixtures._
import org.openjdk.jmh.annotations.Benchmark
import zio.Chunk

class ImmutableEncoderBenchmark extends EncoderBenchmark {
  @Benchmark
  override def rfc7541AppendixC_1_1: Chunk[Byte] =
    ImmutableEncoder.encodePositiveInt(0x00, 5, rfc7541AppendixC_1_1_decoded)

  @Benchmark
  override def rfc7541AppendixC_1_2: Chunk[Byte] =
    ImmutableEncoder.encodePositiveInt(0x00, 5, rfc7541AppendixC_1_2_decoded)

  @Benchmark
  override def rfc7541AppendixC_1_3: Chunk[Byte] =
    ImmutableEncoder.encodePositiveInt(0x00, 8, rfc7541AppendixC_1_3_decoded)
}
