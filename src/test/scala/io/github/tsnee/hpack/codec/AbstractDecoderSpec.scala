package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack.HeaderField
import zio.duration._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

abstract class AbstractDecoderSpec(
  suiteName: String,
  decoderFunctions: DecoderBenchmark
) extends DefaultRunnableSpec {
  override def spec = suite(suiteName)(
    test("decoding an empty header block yields an empty HeaderList") {
      val actual = decoderFunctions.decodingAnEmptyHeaderBlockYieldsAnEmptyHeaderList
      assert(actual)(equalTo(Seq.empty))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.1.1") {
      val actual = decoderFunctions.rfc7541AppendixC_1_1
      assert(actual)(equalTo(Right((Fixtures.rfc7541AppendixC_1_1_decoded, 1))))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.1.2") {
      val actual = decoderFunctions.rfc7541AppendixC_1_2
      assert(actual)(equalTo(Right((Fixtures.rfc7541AppendixC_1_2_decoded, 3))))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.1.3") {
      val actual = decoderFunctions.rfc7541AppendixC_1_3
      assert(actual)(equalTo(Right((Fixtures.rfc7541AppendixC_1_3_decoded, 1))))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.1 in one chunk") {
      val actual = decoderFunctions.rfc7541AppendixC_2_1_in_one_chunk
      val expected = List(HeaderField("custom-key", "custom-header"))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.1 in two chunks") {
      val actual = decoderFunctions.rfc7541AppendixC_2_1_in_two_chunks
      val expected = List(HeaderField("custom-key", "custom-header"))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.2") {
      val actual = decoderFunctions.rfc7541AppendixC_2_2
      val expected = List(HeaderField(":path", "/sample/path"))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.3") {
      val actual = decoderFunctions.rfc7541AppendixC_2_3
      val expected = List(HeaderField("password", "secret"))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.4") {
      val actual = decoderFunctions.rfc7541AppendixC_2_4
      val expected = List(HeaderField(":method", "GET"))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.3.1") {
      val actual = decoderFunctions.rfc7541AppendixC_3_1
      val expected = List(
        HeaderField(":method", "GET"),
        HeaderField(":scheme", "http"),
        HeaderField(":path", "/"),
        HeaderField(":authority", "www.example.com")
      )
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.3.2") {
      val actual = decoderFunctions.rfc7541AppendixC_3_2
      val expected = List(
        HeaderField(":method", "GET"),
        HeaderField(":scheme", "http"),
        HeaderField(":path", "/"),
        HeaderField(":authority", "www.example.com"),
        HeaderField("cache-control", "no-cache")
      )
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.3.3") {
      val actual = decoderFunctions.rfc7541AppendixC_3_3
      val expected = List(
        HeaderField(":method", "GET"),
        HeaderField(":scheme", "https"),
        HeaderField(":path", "/index.html"),
        HeaderField(":authority", "www.example.com"),
        HeaderField("custom-key", "custom-value")
      )
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.4.1") {
      val actual = decoderFunctions.rfc7541AppendixC_4_1
      val expected = List(
        HeaderField(":method", "GET"),
        HeaderField(":scheme", "http"),
        HeaderField(":path", "/"),
        HeaderField(":authority", "www.example.com")
      )
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.4.2") {
      val actual = decoderFunctions.rfc7541AppendixC_4_2
      val expected = List(
        HeaderField(":method", "GET"),
        HeaderField(":scheme", "http"),
        HeaderField(":path", "/"),
        HeaderField(":authority", "www.example.com"),
        HeaderField("cache-control", "no-cache")
      )
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.4.3") {
      val actual = decoderFunctions.rfc7541AppendixC_4_3
      val expected = List(
        HeaderField(":method", "GET"),
        HeaderField(":scheme", "https"),
        HeaderField(":path", "/index.html"),
        HeaderField(":authority", "www.example.com"),
        HeaderField("custom-key", "custom-value")
      )
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds)
  )
}
