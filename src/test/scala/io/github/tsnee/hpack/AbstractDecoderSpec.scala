package io.github.tsnee.hpack

import zio.duration._
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._

abstract class AbstractDecoderSpec[T](
  suiteName: String,
  fixture: HpackBenchmark[T],
  testInput: T
) extends DefaultRunnableSpec {

  override def spec = suite(suiteName)(
    test("decoding an empty header block yields an empty HeaderList") {
      val actual = fixture.decodingAnEmptyHeaderBlockYieldsAnEmptyHeaderList(
        testInput
      )
      assert(actual)(equalTo(Seq.empty))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.1.1") {
      val actual = fixture.rfc7541AppendixC_1_1(testInput)
      assert(actual)(equalTo(Right((10, 1))))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.1.2") {
      val actual = fixture.rfc7541AppendixC_1_2(testInput)
      assert(actual)(equalTo(Right((1337, 3))))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.1.3") {
      val actual = fixture.rfc7541AppendixC_1_3(testInput)
      assert(actual)(equalTo(Right((42, 1))))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.1 in one chunk") {
      val actual = fixture.rfc7541AppendixC_2_1_in_one_chunk(testInput)
      val expected = List(HeaderField("custom-key", "custom-header"))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.1 in two chunks") {
      val actual = fixture.rfc7541AppendixC_2_1_in_two_chunks(testInput)
      val expected = List(HeaderField("custom-key", "custom-header"))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.2") {
      val actual = fixture.rfc7541AppendixC_2_2(testInput)
      val expected = List(HeaderField(":path", "/sample/path"))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.3") {
      val actual = fixture.rfc7541AppendixC_2_3(testInput)
      val expected = List(HeaderField("password", "secret"))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.2.4") {
      val actual = fixture.rfc7541AppendixC_2_4(testInput)
      val expected = List(HeaderField(":method", "GET"))
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.3.1") {
      val actual = fixture.rfc7541AppendixC_3_1(testInput)
      val expected = List(
        HeaderField(":method", "GET"),
        HeaderField(":scheme", "http"),
        HeaderField(":path", "/"),
        HeaderField(":authority", "www.example.com")
      )
      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.3.2") {
      val actual = fixture.rfc7541AppendixC_3_2(testInput)
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
      val actual = fixture.rfc7541AppendixC_3_3(testInput)
      val expected = List(
        HeaderField(":method", "GET"),
        HeaderField(":scheme", "https"),
        HeaderField(":path", "/index.html"),
        HeaderField(":authority", "www.example.com"),
        HeaderField("custom-key", "custom-value")
      )
      assert(actual)(equalTo(expected))
//    } @@ timeout(10.seconds),
//    test("RFC 7541 Appendix C.4.1") {
//      val actual = fixture.rfc7541AppendixC_4_1(testInput)
//      val expected = List(
//        HeaderField(":method", "GET"),
//        HeaderField(":scheme", "http"),
//        HeaderField(":path", "/"),
//        HeaderField(":authority", "www.example.com")
//      )
//      assert(actual)(equalTo(expected))
    } @@ timeout(10.seconds)
  )
}
