package io.github.tsnee.hpack.codec.optimized

import io.github.tsnee.hpack._
import io.github.tsnee.hpack.codec.DecoderBenchmark
import io.github.tsnee.hpack.codec.Fixtures._
import io.github.tsnee.hpack.table.DynamicTable
import org.openjdk.jmh.annotations.Benchmark
import zio.Chunk

class MutableDecoderBenchmark extends DecoderBenchmark {
  def emptyCtx = new MutableDecoderContext(DynamicTable(1024))

  def newCtx(tableSize: Int) =
    new MutableDecoderContext(DynamicTable(tableSize))

  @Benchmark
  override def decodingAnEmptyHeaderBlockYieldsAnEmptyHeaderList: Seq[HeaderField] =
    MutableDecoder
      .decode(Chunk.empty, emptyCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_1_1: Either[DecoderError, (Int, Int)] =
    MutableDecoder.decodeInt(0x1F, rfc7541AppendixC_1_1_encoded, 0)

  @Benchmark
  override def rfc7541AppendixC_1_2: Either[DecoderError, (Int, Int)] =
    MutableDecoder.decodeInt(0x1F, rfc7541AppendixC_1_2_encoded, 0)

  @Benchmark
  override def rfc7541AppendixC_1_3: Either[DecoderError, (Int, Int)] =
    MutableDecoder.decodeInt(0xFF, rfc7541AppendixC_1_3_encoded, 0)

  @Benchmark
  override def rfc7541AppendixC_2_1_in_one_chunk: Seq[HeaderField] =
    MutableDecoder
      .decode(
        rfc7541AppendixC_2_1_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_2_1_in_two_chunks: Seq[HeaderField] = {
    val intermediateCtx = MutableDecoder
      .decode(
        rfc7541AppendixC_2_1_encoded.take(16),
        newCtx(1024))
      .getOrElse(emptyCtx)
    MutableDecoder
      .decode(
        rfc7541AppendixC_2_1_encoded.drop(16),
        intermediateCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_2_2: Seq[HeaderField] =
    MutableDecoder
      .decode(
        rfc7541AppendixC_2_2_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_2_3: Seq[HeaderField] =
    MutableDecoder
      .decode(
        rfc7541AppendixC_2_3_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_2_4: Seq[HeaderField] =
    MutableDecoder
      .decode(
        rfc7541AppendixC_2_4_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_3_1: Seq[HeaderField] =
    MutableDecoder
      .decode(
        rfc7541AppendixC_3_1_encoded,
        newCtx(57))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_3_2: Seq[HeaderField] = {
    val intermediateCtx = MutableDecoder
      .decode(
        rfc7541AppendixC_3_1_encoded,
        newCtx(110))
      .getOrElse(emptyCtx)
      .headerList
      ._2
    MutableDecoder
      .decode(
        rfc7541AppendixC_3_2_encoded,
        intermediateCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_3_3: Seq[HeaderField] = {
    val afterFirstCtx = MutableDecoder
      .decode(
        rfc7541AppendixC_3_1_encoded,
        newCtx(164))
      .getOrElse(emptyCtx)
      .headerList
      ._2
    val afterSecondCtx = MutableDecoder
      .decode(
        rfc7541AppendixC_3_2_encoded,
        afterFirstCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._2
    MutableDecoder
      .decode(
        rfc7541AppendixC_3_3_encoded,
        afterSecondCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }

  override def rfc7541AppendixC_4_1: Seq[HeaderField] =
    MutableDecoder
      .decode(
        rfc7541AppendixC_4_1_encoded,
        newCtx(57))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_4_2: Seq[HeaderField] = {
    val intermediateCtx = MutableDecoder
      .decode(
        rfc7541AppendixC_4_1_encoded,
        newCtx(110))
      .getOrElse(emptyCtx)
      .headerList
      ._2
    MutableDecoder
      .decode(
        rfc7541AppendixC_4_2_encoded,
        intermediateCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_4_3: Seq[HeaderField] = {
    val afterFirstCtx = MutableDecoder
      .decode(
        rfc7541AppendixC_4_1_encoded,
        newCtx(164))
      .getOrElse(emptyCtx)
      .headerList
      ._2
    val afterSecondCtx = MutableDecoder
      .decode(
        rfc7541AppendixC_4_2_encoded,
        afterFirstCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._2
    MutableDecoder
      .decode(
        rfc7541AppendixC_4_3_encoded,
        afterSecondCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }
}
