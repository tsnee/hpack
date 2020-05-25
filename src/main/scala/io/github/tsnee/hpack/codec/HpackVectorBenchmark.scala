package io.github.tsnee.hpack.codec

import scala.language.implicitConversions
import io.github.tsnee.hpack.{HeaderField, HpackError}
import io.github.tsnee.hpack.table.DynamicTable
import org.openjdk.jmh.annotations.Benchmark
import zio.Chunk

object HpackVectorBenchmark {
  implicit def forConvenience(i: Int): Byte = i.toByte

  val emptyCtx = VectorDecoderContext(DynamicTable(1024))
  val rfc7541AppendixC_1_1_encoded: Vector[Byte] =
    Vector(0x0A)
  val rfc7541AppendixC_1_2_encoded: Vector[Byte] =
    Vector(0xFF, 0x9A, 0x0A)
  val rfc7541AppendixC_1_3_encoded: Vector[Byte] =
    Vector(0x2A)
  val rfc7541AppendixC_2_1_encoded: Chunk[Byte] =
    Chunk(
      0x40, 0x0A, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x6B, 0x65,
      0x79, 0x0D, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x68, 0x65,
      0x61, 0x64, 0x65, 0x72
    )
  val rfc7541AppendixC_2_2_encoded: Chunk[Byte] =
    Chunk(
      0x04, 0x0C, 0x2F, 0x73, 0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2F, 0x70,
      0x61, 0x74, 0x68
    )
  val rfc7541AppendixC_2_3_encoded: Chunk[Byte] =
    Chunk(
      0x10, 0x08, 0x70, 0x61, 0x73, 0x73, 0x77, 0x6F, 0x72, 0x64, 0x06,
      0x73, 0x65, 0x63, 0x72, 0x65, 0x74
    )
  val rfc7541AppendixC_2_4_encoded: Chunk[Byte] =
    Chunk(0x82)
  val rfc7541AppendixC_3_1_encoded: Chunk[Byte] =
    Chunk(
      0x82, 0x86, 0x84, 0x41, 0x0F, 0x77, 0x77, 0x77, 0x2E, 0x65, 0x78,
      0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2E, 0x63, 0x6F, 0x6D
    )
  val rfc7541AppendixC_3_2_encoded: Chunk[Byte] =
    Chunk(
      0x82, 0x86, 0x84, 0xBE, 0x58, 0x08, 0x6E, 0x6F, 0x2D, 0x63, 0x61,
      0x63, 0x68, 0x65
    )
  val rfc7541AppendixC_3_3_encoded: Chunk[Byte] =
    Chunk(
      0x82, 0x87, 0x85, 0xBF, 0x40, 0x0A, 0x63, 0x75, 0x73, 0x74, 0x6F,
      0x6D, 0x2D, 0x6B, 0x65, 0x79, 0x0C, 0x63, 0x75, 0x73, 0x74, 0x6F,
      0x6D, 0x2D, 0x76, 0x61, 0x6C, 0x75, 0x65
    )
  val rfc7541AppendixC_4_1_encoded: Chunk[Byte] =
    Chunk(
      0x82, 0x86, 0x84, 0x41, 0x8C, 0xF1, 0xE3, 0xC2, 0xE5, 0xF2, 0x3A,
      0x6B, 0xA0, 0xAB, 0x90, 0xF4, 0xFF
    )
  val rfc7541AppendixC_4_2_encoded: Chunk[Byte] =
    Chunk(
      0x82, 0x86, 0x84, 0xBE, 0x58, 0x86, 0xA8, 0xEB, 0x10, 0x64, 0x9C,
      0xBF
    )
  val rfc7541AppendixC_4_3_encoded: Chunk[Byte] =
    Chunk(
      0x82, 0x87, 0x85, 0xBF, 0x40, 0x88, 0x25, 0xA8, 0x49, 0xE9, 0x5B,
      0xA9, 0x7D, 0x7F, 0x89, 0x25, 0xA8, 0x49, 0xE9, 0x5B, 0xB8, 0xE8,
      0xB4, 0xBF
    )
}

class HpackVectorBenchmark extends HpackBenchmark {
  import HpackVectorBenchmark._

  def newCtx(tableSize: Int) =
    VectorDecoderContext(DynamicTable(tableSize))

  @Benchmark
  override def decodingAnEmptyHeaderBlockYieldsAnEmptyHeaderList: Seq[HeaderField] =
    VectorDecoder
      .decode(Chunk.empty, emptyCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_1_1: Either[HpackError, (Int, Int)] =
    VectorDecoder.decodeInt(0x1F, rfc7541AppendixC_1_1_encoded, 0)

  @Benchmark
  override def rfc7541AppendixC_1_2: Either[HpackError, (Int, Int)] =
    VectorDecoder.decodeInt(0x1F, rfc7541AppendixC_1_2_encoded, 0)

  @Benchmark
  override def rfc7541AppendixC_1_3: Either[HpackError, (Int, Int)] =
    VectorDecoder.decodeInt(0xFF, rfc7541AppendixC_1_3_encoded, 0)

  @Benchmark
  override def rfc7541AppendixC_2_1_in_one_chunk: Seq[HeaderField] =
    VectorDecoder
      .decode(
        rfc7541AppendixC_2_1_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_2_1_in_two_chunks: Seq[HeaderField] = {
    val intermediateCtx = VectorDecoder
      .decode(
        rfc7541AppendixC_2_1_encoded.take(16),
        newCtx(1024))
      .getOrElse(emptyCtx)
    VectorDecoder
      .decode(
        rfc7541AppendixC_2_1_encoded.drop(16),
        intermediateCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_2_2: Seq[HeaderField] =
    VectorDecoder
      .decode(
        rfc7541AppendixC_2_2_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_2_3: Seq[HeaderField] =
    VectorDecoder
      .decode(
        rfc7541AppendixC_2_3_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_2_4: Seq[HeaderField] =
    VectorDecoder
      .decode(
        rfc7541AppendixC_2_4_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_3_1: Seq[HeaderField] =
    VectorDecoder
      .decode(
        rfc7541AppendixC_3_1_encoded,
        newCtx(57))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_3_2: Seq[HeaderField] = {
    val intermediateCtx = VectorDecoder
      .decode(
        rfc7541AppendixC_3_1_encoded,
        newCtx(110))
      .getOrElse(emptyCtx)
      .headerList
      ._2
    VectorDecoder
      .decode(
        rfc7541AppendixC_3_2_encoded,
        intermediateCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_3_3: Seq[HeaderField] = {
    val afterFirstCtx = VectorDecoder
      .decode(
        rfc7541AppendixC_3_1_encoded,
        newCtx(164))
      .getOrElse(emptyCtx)
      .headerList
      ._2
    val afterSecondCtx = VectorDecoder
      .decode(
        rfc7541AppendixC_3_2_encoded,
        afterFirstCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._2
    VectorDecoder
      .decode(
        rfc7541AppendixC_3_3_encoded,
        afterSecondCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_4_1: Seq[HeaderField] =
    VectorDecoder
      .decode(
        rfc7541AppendixC_4_1_encoded,
        newCtx(57))
      .getOrElse(emptyCtx)
      .headerList
      ._1

  @Benchmark
  override def rfc7541AppendixC_4_2: Seq[HeaderField] = {
    val intermediateCtx = VectorDecoder
      .decode(
        rfc7541AppendixC_4_1_encoded,
        newCtx(110))
      .getOrElse(emptyCtx)
      .headerList
      ._2
    VectorDecoder
      .decode(
        rfc7541AppendixC_4_2_encoded,
        intermediateCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }

  @Benchmark
  override def rfc7541AppendixC_4_3: Seq[HeaderField] = {
    val afterFirstCtx = VectorDecoder
      .decode(
        rfc7541AppendixC_4_1_encoded,
        newCtx(164))
      .getOrElse(emptyCtx)
      .headerList
      ._2
    val afterSecondCtx = VectorDecoder
      .decode(
        rfc7541AppendixC_4_2_encoded,
        afterFirstCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._2
    VectorDecoder
      .decode(
        rfc7541AppendixC_4_3_encoded,
        afterSecondCtx)
      .getOrElse(emptyCtx)
      .headerList
      ._1
  }
}
