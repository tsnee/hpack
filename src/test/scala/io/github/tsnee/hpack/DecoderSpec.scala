package io.github.tsnee.hpack

import scala.language.implicitConversions
import zio.Chunk
import zio.duration._
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._

object DecoderSpec extends DefaultRunnableSpec {
  implicit def forConvenience(i: Int): Byte = i.toByte

  override def spec = suite("DecoderSpec")(
    test("decoding an empty header block yields an empty HeaderList") {
      val emptyCtx = DecoderContext.default(1024)
      val newCtx = VectorDecoder.decode(Chunk(), emptyCtx)
      assert(newCtx.headerList)(equalTo(Seq.empty))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.1.1") {
      val mask = 0x1F
      val bytes: Vector[Byte] = Vector(0x0A)
      val actual = VectorDecoder.decodeInt(mask, bytes, 0)
      assert(actual)(equalTo(Right((10, 1))))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.1.2") {
      val mask = 0x1F
      val bytes: Vector[Byte] = Vector(0xFF, 0x9A, 0x0A)
      val actual = VectorDecoder.decodeInt(mask, bytes, 0)
      assert(actual)(equalTo(Right((1337, 3))))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.1.3") {
      val mask = 0xFF
      val bytes: Vector[Byte] = Vector(0x2A)
      val actual = VectorDecoder.decodeInt(mask, bytes, 0)
      assert(actual)(equalTo(Right((42, 1))))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.1 in one chunk") {
      val chunk: Chunk[Byte] = Chunk(
        0x40, 0x0A, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x6B, 0x65,
        0x79, 0x0D, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x68, 0x65,
        0x61, 0x64, 0x65, 0x72
      )
      val actual =
        Decoder.default.decode(chunk, DecoderContext.default(1024)).headerList
      val expected =
        List(HeaderField("custom-key", "custom-header", Indexing.With))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.1 in two chunks") {
      val first: Chunk[Byte] = Chunk(
        0x40, 0x0A, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x6B, 0x65
      )
      val second: Chunk[Byte] = Chunk(
        0x79, 0x0D, 0x63, 0x75, 0x73, 0x74, 0x6F, 0x6D, 0x2D, 0x68, 0x65,
        0x61, 0x64, 0x65, 0x72
      )
      val intermediateCtx =
        Decoder.default.decode(first, DecoderContext.default(1024))
      val actual =
        Decoder.default.decode(second, intermediateCtx).headerList
      val expected =
        List(HeaderField("custom-key", "custom-header", Indexing.With))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.2") {
      val chunk: Chunk[Byte] = Chunk(
        0x04, 0x0C, 0x2F, 0x73, 0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2F, 0x70,
        0x61, 0x74, 0x68
      )
      val actual =
        Decoder.default.decode(chunk, DecoderContext.default(1024)).headerList
      val expected = List(HeaderField(":path", "/sample/path", Indexing.Without))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.3") {
      val chunk: Chunk[Byte] = Chunk(
        0x10, 0x08, 0x70, 0x61, 0x73, 0x73, 0x77, 0x6F, 0x72, 0x64, 0x06,
        0x73, 0x65, 0x63, 0x72, 0x65, 0x74
      )
      val actual =
        Decoder.default.decode(chunk, DecoderContext.default(1024)).headerList
      val expected =
        List(HeaderField("password", "secret", Indexing.Never))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.4") {
      val chunk: Chunk[Byte] = Chunk(0x82)
      val actual =
        Decoder.default.decode(chunk, DecoderContext.default(1024)).headerList
      val expected =
        List(HeaderField(":method", "GET", Indexing.Without))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.3.1") {
      val chunk: Chunk[Byte] = Chunk(
        0x82, 0x86, 0x84, 0x41, 0x0F, 0x77, 0x77, 0x77, 0x2E, 0x65, 0x78,
        0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2E, 0x63, 0x6F, 0x6D
      )
      val actual =
        Decoder.default.decode(chunk, DecoderContext.default(57)).headerList
      val expected = List(
        HeaderField(":method", "GET", Indexing.Without),
        HeaderField(":scheme", "http", Indexing.Without),
        HeaderField(":path", "/", Indexing.Without),
        HeaderField(":authority", "www.example.com", Indexing.With)
      )
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.3.2") {
      val first: Chunk[Byte] = Chunk(
        0x82, 0x86, 0x84, 0x41, 0x0F, 0x77, 0x77, 0x77, 0x2E, 0x65, 0x78,
        0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2E, 0x63, 0x6F, 0x6D
      )
      val second: Chunk[Byte] = Chunk(
        0x82, 0x86, 0x84, 0xBE, 0x58, 0x08, 0x6E, 0x6F, 0x2D, 0x63, 0x61,
        0x63, 0x68, 0x65
      )
      val intermediateCtx =
        Decoder.default.decode(first, DecoderContext.default(110)).flush
      val actual =
        Decoder.default.decode(second, intermediateCtx).headerList
      Console.err.println(actual.map {
        case HeaderField(name, value, indexing) =>
          new String(name.toArray) + " " + new String(value.toArray)
      }.mkString("\n"))
      val expected = List(
        HeaderField(":method", "GET", Indexing.Without),
        HeaderField(":scheme", "http", Indexing.Without),
        HeaderField(":path", "/", Indexing.Without),
        HeaderField(":authority", "www.example.com", Indexing.With),
        HeaderField("cache-control", "no-cache", Indexing.With)
      )
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.3.3") {
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
        Decoder.default.decode(first, DecoderContext.default(164)).flush
      val afterSecondCtx =
        Decoder.default.decode(second, afterFirstCtx).flush
      val actual =
        Decoder.default.decode(third, afterSecondCtx).headerList
      Console.err.println(actual.map {
        case HeaderField(name, value, indexing) =>
          new String(name.toArray) + " " + new String(value.toArray)
      }.mkString("\n"))
      val expected = List(
        HeaderField(":method", "GET", Indexing.Without),
        HeaderField(":scheme", "https", Indexing.Without),
        HeaderField(":path", "/index.html", Indexing.Without),
        HeaderField(":authority", "www.example.com", Indexing.With),
        HeaderField("custom-key", "custom-value", Indexing.With)
      )
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds)
  )
}
