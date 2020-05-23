package io.github.tsnee.hpack

import scala.language.implicitConversions
import org.openjdk.jmh.annotations.Benchmark
import zio.Chunk

class HpackVectorBenchmark extends HpackBenchmark {
  implicit def forConvenience(i: Int): Byte = i.toByte

  @Benchmark
  override def decodingAnEmptyHeaderBlockYieldsAnEmptyHeaderList = {
    val emptyCtx = VectorDecoderContext(DynamicTable(1024))
    VectorDecoder.decode(Chunk.empty, emptyCtx).headerList
  }

  @Benchmark
  override def rfc7541AppendixC_1_1 = {
    val mask = 0x1F
    val bytes: Vector[Byte] = Vector(0x0A)
    VectorDecoder.decodeInt(mask, bytes, 0)
  }

  @Benchmark
  override def rfc7541AppendixC_1_2 = {
    val mask = 0x1F
    val bytes: Vector[Byte] = Vector(0xFF, 0x9A, 0x0A)
    VectorDecoder.decodeInt(mask, bytes, 0)
  }

  @Benchmark
  override def rfc7541AppendixC_1_3 = {
    val mask = 0xFF
    val bytes: Vector[Byte] = Vector(0x2A)
    VectorDecoder.decodeInt(mask, bytes, 0)
  }

  @Benchmark
  override def rfc7541AppendixC_2_1_in_one_chunk = {
    val chunk: Chunk[Byte] = Chunk(
      0x40, 0x0A, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x6B, 0x65,
      0x79, 0x0D, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x68, 0x65,
      0x61, 0x64, 0x65, 0x72
    )
    VectorDecoder
      .decode(chunk, VectorDecoderContext(DynamicTable(1024)))
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_2_1_in_two_chunks = {
    val first: Chunk[Byte] = Chunk(
      0x40, 0x0A, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x6B, 0x65
    )
    val second: Chunk[Byte] = Chunk(
      0x79, 0x0D, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x68, 0x65,
      0x61, 0x64, 0x65, 0x72
    )
    val intermediateCtx =
      VectorDecoder.decode(first, VectorDecoderContext(DynamicTable(1024)))
    VectorDecoder.decode(second, intermediateCtx).headerList
  }

  @Benchmark
  override def rfc7541AppendixC_2_2 = {
    val chunk: Chunk[Byte] = Chunk(
      0x04, 0x0C, 0x2F, 0x73, 0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2F, 0x70,
      0x61, 0x74, 0x68
    )
    VectorDecoder
      .decode(chunk, VectorDecoderContext(DynamicTable(1024)))
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_2_3 = {
    val chunk: Chunk[Byte] = Chunk(
      0x10, 0x08, 0x70, 0x61, 0x73, 0x73, 0x77, 0x6F, 0x72, 0x64, 0x06,
      0x73, 0x65, 0x63, 0x72, 0x65, 0x74
    )
    VectorDecoder
      .decode(chunk, VectorDecoderContext(DynamicTable(1024)))
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_2_4 = {
    val chunk: Chunk[Byte] = Chunk.single(0x82)
    VectorDecoder
      .decode(chunk, VectorDecoderContext(DynamicTable(1024)))
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_3_1 = {
    val chunk: Chunk[Byte] = Chunk(
      0x82, 0x86, 0x84, 0x41, 0x0F, 0x77, 0x77, 0x77, 0x2E, 0x65, 0x78,
      0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2E, 0x63, 0x6F, 0x6D
    )
    VectorDecoder
      .decode(chunk, VectorDecoderContext(DynamicTable(57)))
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_3_2 = {
    val first: Chunk[Byte] = Chunk(
      0x82, 0x86, 0x84, 0x41, 0x0F, 0x77, 0x77, 0x77, 0x2E, 0x65, 0x78,
      0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2E, 0x63, 0x6F, 0x6D
    )
    val second: Chunk[Byte] = Chunk(
      0x82, 0x86, 0x84, 0xBE, 0x58, 0x08, 0x6E, 0x6F, 0x2D, 0x63, 0x61,
      0x63, 0x68, 0x65
    )
    val intermediateCtx =
      VectorDecoder
        .decode(first, VectorDecoderContext(DynamicTable(110)))
        .flush
    VectorDecoder.decode(second, intermediateCtx).headerList
  }

  @Benchmark
  override def rfc7541AppendixC_3_3 = {
    val first: Chunk[Byte] = Chunk(
      0x82, 0x86, 0x84, 0x41, 0x0F, 0x77, 0x77, 0x77, 0x2E, 0x65, 0x78,
      0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2E, 0x63, 0x6F, 0x6D
    )
    val second: Chunk[Byte] = Chunk(
      0x82, 0x86, 0x84, 0xBE, 0x58, 0x08, 0x6E, 0x6F, 0x2D, 0x63, 0x61,
      0x63, 0x68, 0x65
    )
    val third: Chunk[Byte] = Chunk(
      0x82, 0x87, 0x85, 0xBF, 0x40, 0x0A, 0x63, 0x75, 0x73, 0x74, 0x6F,
      0x6D, 0x2D, 0x6B, 0x65, 0x79, 0x0C, 0x63, 0x75, 0x73, 0x74, 0x6F,
      0x6D, 0x2D, 0x76, 0x61, 0x6C, 0x75, 0x65
    )
    val afterFirstCtx =
      VectorDecoder
        .decode(first, VectorDecoderContext(DynamicTable(164)))
        .flush
    val afterSecondCtx =
      VectorDecoder.decode(second, afterFirstCtx).flush
    VectorDecoder.decode(third, afterSecondCtx).headerList
  }

  override def rfc7541AppendixC_4_1 = {
    val first: Chunk[Byte] = Chunk(
      0x82, 0x86, 0x84, 0x41, 0x8C, 0xF1, 0xE3, 0xC2, 0xE5, 0xF2, 0x3A,
      0x6B, 0xA0, 0xAB, 0x90, 0xF4, 0xFF
    )
    VectorDecoder
      .decode(first, VectorDecoderContext(DynamicTable(57)))
      .headerList
  }
}
