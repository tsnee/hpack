package io.github.tsnee.hpack

import scala.language.implicitConversions
import org.openjdk.jmh.annotations.Benchmark
import zio.Chunk

class HpackChunkBenchmark extends HpackBenchmark {
  implicit def forConvenience(i: Int): Byte = i.toByte

  @Benchmark
  override def decodingAnEmptyHeaderBlockYieldsAnEmptyHeaderList = {
    val emptyCtx = new ChunkDecoderContext(DynamicTable(1024))
    ChunkDecoder
      .decode(Chunk.empty, emptyCtx)
      .getOrElse(emptyCtx)
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_1_1 = {
    val mask = 0x1F
    val bytes: Chunk[Byte] = Chunk.single(0x0A)
    ChunkDecoder.decodeInt(mask, bytes, 0)
  }

  @Benchmark
  override def rfc7541AppendixC_1_2 = {
    val mask = 0x1F
    val bytes: Chunk[Byte] = Chunk(0xFF, 0x9A, 0x0A)
    ChunkDecoder.decodeInt(mask, bytes, 0)
  }

  @Benchmark
  override def rfc7541AppendixC_1_3 = {
    val mask = 0xFF
    val bytes: Chunk[Byte] = Chunk.single(0x2A)
    ChunkDecoder.decodeInt(mask, bytes, 0)
  }

  @Benchmark
  override def rfc7541AppendixC_2_1_in_one_chunk = {
    val emptyCtx = new ChunkDecoderContext(DynamicTable(1024))
    val chunk: Chunk[Byte] = Chunk(
      0x40, 0x0A, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x6B, 0x65,
      0x79, 0x0D, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x68, 0x65,
      0x61, 0x64, 0x65, 0x72
    )
    ChunkDecoder
      .decode(chunk, new ChunkDecoderContext(DynamicTable(1024)))
      .getOrElse(emptyCtx)
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_2_1_in_two_chunks = {
    val emptyCtx = new ChunkDecoderContext(DynamicTable(1024))
    val first: Chunk[Byte] = Chunk(
      0x40, 0x0A, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x6B, 0x65
    )
    val second: Chunk[Byte] = Chunk(
      0x79, 0x0D, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x68, 0x65,
      0x61, 0x64, 0x65, 0x72
    )
    val intermediateCtx = ChunkDecoder
      .decode(first, new ChunkDecoderContext(DynamicTable(1024)))
      .getOrElse(emptyCtx)
    ChunkDecoder
      .decode(second, intermediateCtx)
      .getOrElse(emptyCtx)
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_2_2 = {
    val emptyCtx = new ChunkDecoderContext(DynamicTable(1024))
    val chunk: Chunk[Byte] = Chunk(
      0x04, 0x0C, 0x2F, 0x73, 0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2F, 0x70,
      0x61, 0x74, 0x68
    )
    ChunkDecoder
      .decode(chunk, new ChunkDecoderContext(DynamicTable(1024)))
      .getOrElse(emptyCtx)
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_2_3 = {
    val emptyCtx = new ChunkDecoderContext(DynamicTable(1024))
    val chunk: Chunk[Byte] = Chunk(
      0x10, 0x08, 0x70, 0x61, 0x73, 0x73, 0x77, 0x6F, 0x72, 0x64, 0x06,
      0x73, 0x65, 0x63, 0x72, 0x65, 0x74
    )
    ChunkDecoder
      .decode(chunk, new ChunkDecoderContext(DynamicTable(1024)))
      .getOrElse(emptyCtx)
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_2_4 = {
    val emptyCtx = new ChunkDecoderContext(DynamicTable(1024))
    val chunk: Chunk[Byte] = Chunk.single(0x82)
    ChunkDecoder
      .decode(chunk, new ChunkDecoderContext(DynamicTable(1024)))
      .getOrElse(emptyCtx)
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_3_1 = {
    val emptyCtx = new ChunkDecoderContext(DynamicTable(1024))
    val chunk: Chunk[Byte] = Chunk(
      0x82, 0x86, 0x84, 0x41, 0x0F, 0x77, 0x77, 0x77, 0x2E, 0x65, 0x78,
      0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2E, 0x63, 0x6F, 0x6D
    )
    ChunkDecoder
      .decode(chunk, new ChunkDecoderContext(DynamicTable(57)))
      .getOrElse(emptyCtx)
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_3_2 = {
    val emptyCtx = new ChunkDecoderContext(DynamicTable(1024))
    val first: Chunk[Byte] = Chunk(
      0x82, 0x86, 0x84, 0x41, 0x0F, 0x77, 0x77, 0x77, 0x2E, 0x65, 0x78,
      0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2E, 0x63, 0x6F, 0x6D
    )
    val second: Chunk[Byte] = Chunk(
      0x82, 0x86, 0x84, 0xBE, 0x58, 0x08, 0x6E, 0x6F, 0x2D, 0x63, 0x61,
      0x63, 0x68, 0x65
    )
    val intermediateCtx = ChunkDecoder
      .decode(first, new ChunkDecoderContext(DynamicTable(110)))
      .getOrElse(emptyCtx)
      .asInstanceOf[ChunkDecoderContext]
    intermediateCtx.headers = Nil
    ChunkDecoder
      .decode(second, intermediateCtx)
      .getOrElse(emptyCtx)
      .headerList
  }

  @Benchmark
  override def rfc7541AppendixC_3_3 = {
    val emptyCtx = new ChunkDecoderContext(DynamicTable(1024))
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
    val afterFirstCtx = ChunkDecoder
      .decode(first, new ChunkDecoderContext(DynamicTable(164)))
      .getOrElse(emptyCtx)
      .asInstanceOf[ChunkDecoderContext]
    afterFirstCtx.headers = Nil
    val afterSecondCtx = ChunkDecoder
      .decode(second, afterFirstCtx)
      .getOrElse(emptyCtx)
      .asInstanceOf[ChunkDecoderContext]
    afterSecondCtx.headers = Nil
    ChunkDecoder
      .decode(third, afterSecondCtx)
      .getOrElse(emptyCtx)
      .headerList
  }

  override def rfc7541AppendixC_4_1 = {
    val emptyCtx = new ChunkDecoderContext(DynamicTable(1024))
    val first: Chunk[Byte] = Chunk(
      0x82, 0x86, 0x84, 0x41, 0x8C, 0xF1, 0xE3, 0xC2, 0xE5, 0xF2, 0x3A,
      0x6B, 0xA0, 0xAB, 0x90, 0xF4, 0xFF
    )
    ChunkDecoder
      .decode(first, new ChunkDecoderContext(DynamicTable(57)))
      .getOrElse(emptyCtx)
      .headerList
  }
}
