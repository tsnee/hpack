package io.github.tsnee.hpack.codec.functional

import io.github.tsnee.hpack.codec.EncoderBenchmark
import io.github.tsnee.hpack.codec.Fixtures._
import io.github.tsnee.hpack.table.DynamicTable
import org.openjdk.jmh.annotations.Benchmark
import zio.Chunk

class ImmutableEncoderBenchmark extends EncoderBenchmark {
  import ImmutableEncoderBenchmark._

  @Benchmark
  override def rfc7541AppendixC_1_1: Chunk[Byte] =
    ImmutableEncoder.encodeNonNegativeInt(0x00, 5, rfc7541AppendixC_1_1_decoded)

  @Benchmark
  override def rfc7541AppendixC_1_2: Chunk[Byte] =
    ImmutableEncoder.encodeNonNegativeInt(0x00, 5, rfc7541AppendixC_1_2_decoded)

  @Benchmark
  override def rfc7541AppendixC_1_3: Chunk[Byte] =
    ImmutableEncoder.encodeNonNegativeInt(0x00, 8, rfc7541AppendixC_1_3_decoded)

  @Benchmark
  override def rfc7541AppendixC_5_1: Chunk[Byte] =
    ImmutableEncoder
      .encode(
        rfc7541AppendixC_5_1_decoded,
        newCtx(256, doNotCompress)
      )
      .headerBlock
      ._1

  @Benchmark
  override def rfc7541AppendixC_5_2: Chunk[Byte] = {
    val afterFirstCtx = ImmutableEncoder
      .encode(
        rfc7541AppendixC_5_1_decoded,
        newCtx(256, doNotCompress)
      )
      .headerBlock
      ._2
    ImmutableEncoder
      .encode(
        rfc7541AppendixC_5_2_decoded,
        afterFirstCtx
      )
      .headerBlock
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_5_3: Chunk[Byte] = {
    val afterFirstCtx = ImmutableEncoder
      .encode(
        rfc7541AppendixC_5_1_decoded,
        newCtx(256, doNotCompress)
      )
      .headerBlock
      ._2
    val afterSecondCtx = ImmutableEncoder
      .encode(
        rfc7541AppendixC_5_2_decoded,
        afterFirstCtx
      )
      .headerBlock
      ._2
    ImmutableEncoder
      .encode(
        rfc7541AppendixC_5_3_decoded,
        afterSecondCtx
      )
      .headerBlock
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_6_1: Chunk[Byte] =
    ImmutableEncoder
      .encode(
        rfc7541AppendixC_6_1_decoded,
        newCtx(256, compress)
      )
      .headerBlock
      ._1

  @Benchmark
  override def rfc7541AppendixC_6_2: Chunk[Byte] = {
    val afterFirstCtx = ImmutableEncoder
      .encode(
        rfc7541AppendixC_6_1_decoded,
        newCtx(256, compress)
      )
      .headerBlock
      ._2
    ImmutableEncoder
      .encode(
        rfc7541AppendixC_6_2_decoded,
        afterFirstCtx
      )
      .headerBlock
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_6_3: Chunk[Byte] = {
    val afterFirstCtx = ImmutableEncoder
      .encode(
        rfc7541AppendixC_6_1_decoded,
        newCtx(256, compress)
      )
      .headerBlock
      ._2
    val afterSecondCtx = ImmutableEncoder
      .encode(
        rfc7541AppendixC_6_2_decoded,
        afterFirstCtx
      )
      .headerBlock
      ._2
    ImmutableEncoder
      .encode(
        rfc7541AppendixC_6_3_decoded,
        afterSecondCtx
      )
      .headerBlock
      ._1
  }
}

object ImmutableEncoderBenchmark {
  def newCtx(tableSize: Int, compress: Boolean): ImmutableEncoderContext =
    ImmutableEncoderContext(
      table = DynamicTable(tableSize),
      compressedByDefault = compress
    )

  val compress = true
  val doNotCompress = false
}
