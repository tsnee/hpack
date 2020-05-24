package io.github.tsnee.hpack

import scala.language.implicitConversions
import org.openjdk.jmh.annotations.Benchmark
import zio.Chunk

object HpackVectorBenchmark {
  implicit def forConvenience(i: Int): Byte = i.toByte

  val emptyCtx = new VectorDecoderContext(DynamicTable(1024))
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
}

class HpackVectorBenchmark extends HpackBenchmark {
  import HpackVectorBenchmark._

  implicit def forConvenience(i: Int): Byte = i.toByte

  def newCtx(tableSize: Int) =
    new VectorDecoderContext(DynamicTable(tableSize))

  @Benchmark
  override def decodingAnEmptyHeaderBlockYieldsAnEmptyHeaderList =
    VectorDecoder
      .decode(Chunk.empty, emptyCtx)
      .getOrElse(emptyCtx)
      .headerList

  @Benchmark
  override def rfc7541AppendixC_1_1 =
    VectorDecoder.decodeInt(0x1F, rfc7541AppendixC_1_1_encoded, 0)

  @Benchmark
  override def rfc7541AppendixC_1_2 =
    VectorDecoder.decodeInt(0x1F, rfc7541AppendixC_1_2_encoded, 0)

  @Benchmark
  override def rfc7541AppendixC_1_3 =
    VectorDecoder.decodeInt(0xFF, rfc7541AppendixC_1_3_encoded, 0)

  @Benchmark
  override def rfc7541AppendixC_2_1_in_one_chunk =
    VectorDecoder
      .decode(
        rfc7541AppendixC_2_1_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList

  @Benchmark
  override def rfc7541AppendixC_2_1_in_two_chunks = {
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
  }

  @Benchmark
  override def rfc7541AppendixC_2_2 =
    VectorDecoder
      .decode(
        rfc7541AppendixC_2_2_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList

  @Benchmark
  override def rfc7541AppendixC_2_3 =
    VectorDecoder
      .decode(
        rfc7541AppendixC_2_3_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList

  @Benchmark
  override def rfc7541AppendixC_2_4 =
    VectorDecoder
      .decode(
        rfc7541AppendixC_2_4_encoded,
        newCtx(1024))
      .getOrElse(emptyCtx)
      .headerList

  @Benchmark
  override def rfc7541AppendixC_3_1 =
    VectorDecoder
      .decode(
        rfc7541AppendixC_3_1_encoded,
        newCtx(57))
      .getOrElse(emptyCtx)
      .headerList

  @Benchmark
  override def rfc7541AppendixC_3_2 = {
    val intermediateCtx = VectorDecoder
      .decode(
        rfc7541AppendixC_3_1_encoded,
        newCtx(110))
      .getOrElse(emptyCtx)
      .asInstanceOf[VectorDecoderContext]
      .copy(headers =Nil)
    VectorDecoder
      .decode(
        rfc7541AppendixC_3_2_encoded,
        intermediateCtx)
      .getOrElse(emptyCtx)
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_3_3 = {
    val afterFirstCtx = VectorDecoder
      .decode(
        rfc7541AppendixC_3_1_encoded,
        newCtx(164))
      .getOrElse(emptyCtx)
      .asInstanceOf[VectorDecoderContext]
      .copy(headers = Nil)
    val afterSecondCtx = VectorDecoder
      .decode(
        rfc7541AppendixC_3_2_encoded,
        afterFirstCtx)
      .getOrElse(emptyCtx)
      .asInstanceOf[VectorDecoderContext]
      .copy(headers = Nil)
    VectorDecoder
      .decode(
        rfc7541AppendixC_3_3_encoded,
        afterSecondCtx)
      .getOrElse(emptyCtx)
      .headerList
  }

  override def rfc7541AppendixC_4_1 =
    VectorDecoder
      .decode(
        rfc7541AppendixC_4_1_encoded,
        newCtx(57))
      .getOrElse(emptyCtx)
      .headerList
}
