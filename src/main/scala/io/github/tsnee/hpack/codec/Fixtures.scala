package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack._
import zio.Chunk

object Fixtures {
  val rfc7541AppendixC_1_1_encoded: Chunk[Byte] = Chunk.single(0x0A)
  val rfc7541AppendixC_1_1_decoded: Int = 10
  val rfc7541AppendixC_1_2_encoded: Chunk[Byte] = Chunk(0x1F, 0x9A, 0x0A)
  val rfc7541AppendixC_1_2_decoded: Int = 1337
  val rfc7541AppendixC_1_3_encoded: Chunk[Byte] = Chunk.single(0x2A)
  val rfc7541AppendixC_1_3_decoded: Int = 42
  val rfc7541AppendixC_2_1_encoded: Chunk[Byte] = Chunk(
    0x40, 0x0A, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x6B, 0x65,
    0x79, 0x0D, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x68, 0x65,
    0x61, 0x64, 0x65, 0x72
  )
  val rfc7541AppendixC_2_1_decoded: Seq[HeaderField] = List(
    HeaderField("custom-key", "custom-header")
  )
  val rfc7541AppendixC_2_2_encoded: Chunk[Byte] = Chunk(
    0x04, 0x0C, 0x2F, 0x73, 0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2F, 0x70,
    0x61, 0x74, 0x68
  )
  val rfc7541AppendixC_2_2_decoded: Seq[HeaderField] = List(
    HeaderField(":path", "/sample/path")
  )
  val rfc7541AppendixC_2_3_encoded: Chunk[Byte] = Chunk(
    0x10, 0x08, 0x70, 0x61, 0x73, 0x73, 0x77, 0x6F, 0x72, 0x64, 0x06,
    0x73, 0x65, 0x63, 0x72, 0x65, 0x74
  )
  val rfc7541AppendixC_2_3_decoded: Seq[HeaderField] = List(
    HeaderField("password", "secret")
  )
  val rfc7541AppendixC_2_4_encoded: Chunk[Byte] = Chunk.single(0x82)
  val rfc7541AppendixC_2_4_decoded: Seq[HeaderField] = List(
    HeaderField(":method", "GET")
  )
  val rfc7541AppendixC_3_1_encoded: Chunk[Byte] = Chunk(
    0x82, 0x86, 0x84, 0x41, 0x0F, 0x77, 0x77, 0x77, 0x2E, 0x65, 0x78,
    0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2E, 0x63, 0x6F, 0x6D
  )
  val rfc7541AppendixC_3_1_decoded: Seq[HeaderField] = List(
    HeaderField(":method", "GET"),
    HeaderField(":scheme", "http"),
    HeaderField(":path", "/"),
    HeaderField(":authority", "www.example.com")
  )
  val rfc7541AppendixC_3_2_encoded: Chunk[Byte] = Chunk(
    0x82, 0x86, 0x84, 0xBE, 0x58, 0x08, 0x6E, 0x6F, 0x2D, 0x63, 0x61,
    0x63, 0x68, 0x65
  )
  val rfc7541AppendixC_3_2_decoded: Seq[HeaderField] = List(
    HeaderField(":method", "GET"),
    HeaderField(":scheme", "http"),
    HeaderField(":path", "/"),
    HeaderField(":authority", "www.example.com"),
    HeaderField("cache-control", "no-cache")
  )
  val rfc7541AppendixC_3_3_encoded: Chunk[Byte] = Chunk(
    0x82, 0x87, 0x85, 0xBF, 0x40, 0x0A, 0x63, 0x75, 0x73, 0x74, 0x6F,
    0x6D, 0x2D, 0x6B, 0x65, 0x79, 0x0C, 0x63, 0x75, 0x73, 0x74, 0x6F,
    0x6D, 0x2D, 0x76, 0x61, 0x6C, 0x75, 0x65
  )
  val rfc7541AppendixC_3_3_decoded: Seq[HeaderField] = List(
    HeaderField(":method", "GET"),
    HeaderField(":scheme", "https"),
    HeaderField(":path", "/index.html"),
    HeaderField(":authority", "www.example.com"),
    HeaderField("custom-key", "custom-value")
  )
  val rfc7541AppendixC_4_1_encoded: Chunk[Byte] = Chunk(
    0x82, 0x86, 0x84, 0x41, 0x8C, 0xF1, 0xE3, 0xC2, 0xE5, 0xF2, 0x3A,
    0x6B, 0xA0, 0xAB, 0x90, 0xF4, 0xFF
  )
  val rfc7541AppendixC_4_1_decoded: Seq[HeaderField] = List(
    HeaderField(":method", "GET"),
    HeaderField(":scheme", "http"),
    HeaderField(":path", "/"),
    HeaderField(":authority", "www.example.com")
  )
  val rfc7541AppendixC_4_2_encoded: Chunk[Byte] = Chunk(
    0x82, 0x86, 0x84, 0xBE, 0x58, 0x86, 0xA8, 0xEB, 0x10, 0x64, 0x9C,
    0xBF
  )
  val rfc7541AppendixC_4_2_decoded: Seq[HeaderField] = List(
    HeaderField(":method", "GET"),
    HeaderField(":scheme", "http"),
    HeaderField(":path", "/"),
    HeaderField(":authority", "www.example.com"),
    HeaderField("cache-control", "no-cache")
  )
  val rfc7541AppendixC_4_3_encoded: Chunk[Byte] = Chunk(
    0x82, 0x87, 0x85, 0xBF, 0x40, 0x88, 0x25, 0xA8, 0x49, 0xE9, 0x5B,
    0xA9, 0x7D, 0x7F, 0x89, 0x25, 0xA8, 0x49, 0xE9, 0x5B, 0xB8, 0xE8,
    0xB4, 0xBF
  )
  val rfc7541AppendixC_4_3_decoded: Seq[HeaderField] = List(
    HeaderField(":method", "GET"),
    HeaderField(":scheme", "https"),
    HeaderField(":path", "/index.html"),
    HeaderField(":authority", "www.example.com"),
    HeaderField("custom-key", "custom-value")
  )
}