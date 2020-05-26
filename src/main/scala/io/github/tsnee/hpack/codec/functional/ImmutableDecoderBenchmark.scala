package io.github.tsnee.hpack.codec.functional

import io.github.tsnee.hpack._
import io.github.tsnee.hpack.codec.DecoderBenchmark
import io.github.tsnee.hpack.codec.Fixtures._
import io.github.tsnee.hpack.table.DynamicTable
import org.openjdk.jmh.annotations.Benchmark
import zio.Chunk

class ImmutableDecoderBenchmark extends DecoderBenchmark {
  import ImmutableDecoderBenchmark._

  @Benchmark
  override def decodingAnEmptyHeaderBlockYieldsAnEmptyHeaderList: Seq[HeaderField] =
    ImmutableDecoder
      .decode(Chunk.empty, emptyCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_1_1: Either[DecoderError, (Int, Int)] =
    ImmutableDecoder.decodeInt(0x1F, rfc7541AppendixC_1_1_encoded, 0)

  @Benchmark
  override def rfc7541AppendixC_1_2: Either[DecoderError, (Int, Int)] =
    ImmutableDecoder.decodeInt(0x1F, rfc7541AppendixC_1_2_encoded, 0)

  @Benchmark
  override def rfc7541AppendixC_1_3: Either[DecoderError, (Int, Int)] =
    ImmutableDecoder.decodeInt(0xFF, rfc7541AppendixC_1_3_encoded, 0)

  @Benchmark
  override def rfc7541AppendixC_2_1_in_one_chunk: Seq[HeaderField] =
    ImmutableDecoder
      .decode(
        rfc7541AppendixC_2_1_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_2_1_in_two_chunks: Seq[HeaderField] = {
    val intermediateCtx = ImmutableDecoder
      .decode(
        rfc7541AppendixC_2_1_encoded.take(16),
        newCtx(1024))
      .getOrElse(emptyCtx)
    ImmutableDecoder
      .decode(
        rfc7541AppendixC_2_1_encoded.drop(16),
        intermediateCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_2_2: Seq[HeaderField] =
    ImmutableDecoder
      .decode(
        rfc7541AppendixC_2_2_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_2_3: Seq[HeaderField] =
    ImmutableDecoder
      .decode(
        rfc7541AppendixC_2_3_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_2_4: Seq[HeaderField] =
    ImmutableDecoder
      .decode(
        rfc7541AppendixC_2_4_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_3_1: Seq[HeaderField] =
    ImmutableDecoder
      .decode(
        rfc7541AppendixC_3_1_encoded,
        newCtx(57))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_3_2: Seq[HeaderField] = {
    val intermediateCtx = ImmutableDecoder
      .decode(
        rfc7541AppendixC_3_1_encoded,
        newCtx(110))
      .getOrElse(emptyCtx)
      .headerList
      ._2
    ImmutableDecoder
      .decode(
        rfc7541AppendixC_3_2_encoded,
        intermediateCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_3_3: Seq[HeaderField] = {
    val afterFirstCtx = ImmutableDecoder
      .decode(
        rfc7541AppendixC_3_1_encoded,
        newCtx(164))
      .getOrElse(emptyCtx)
      .headerList
      ._2
    val afterSecondCtx = ImmutableDecoder
      .decode(
        rfc7541AppendixC_3_2_encoded,
        afterFirstCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._2
    ImmutableDecoder
      .decode(
        rfc7541AppendixC_3_3_encoded,
        afterSecondCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_4_1: Seq[HeaderField] =
    ImmutableDecoder
      .decode(
        rfc7541AppendixC_4_1_encoded,
        newCtx(57))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_4_2: Seq[HeaderField] = {
    val intermediateCtx = ImmutableDecoder
      .decode(
        rfc7541AppendixC_4_1_encoded,
        newCtx(110))
      .getOrElse(emptyCtx)
      .headerList
      ._2
    ImmutableDecoder
      .decode(
        rfc7541AppendixC_4_2_encoded,
        intermediateCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_4_3: Seq[HeaderField] = {
    val afterFirstCtx = ImmutableDecoder
      .decode(
        rfc7541AppendixC_4_1_encoded,
        newCtx(164))
      .getOrElse(emptyCtx)
      .headerList
      ._2
    val afterSecondCtx = ImmutableDecoder
      .decode(
        rfc7541AppendixC_4_2_encoded,
        afterFirstCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._2
    ImmutableDecoder
      .decode(
        rfc7541AppendixC_4_3_encoded,
        afterSecondCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }
}

object ImmutableDecoderBenchmark {
  val emptyCtx: ImmutableDecoderContext = ImmutableDecoderContext(DynamicTable(1024))

  def newCtx(tableSize: Int): ImmutableDecoderContext =
    ImmutableDecoderContext(DynamicTable(tableSize))
}
