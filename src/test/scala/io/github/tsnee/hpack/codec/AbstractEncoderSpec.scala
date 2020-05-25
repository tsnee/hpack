package io.github.tsnee.hpack.codec

import zio.duration._
import zio.test.Assertion.equalTo
import zio.test.DefaultRunnableSpec
import zio.test.TestAspect.timeout
import zio.test._

class AbstractEncoderSpec(
  suiteName: String,
  encoderFunctions: EncoderBenchmark
) extends DefaultRunnableSpec {

  override def spec = suite(suiteName)(
    test("RFC 7541 Appendix C.1.1") {
      val actual = encoderFunctions.rfc7541AppendixC_1_1
      assert(actual)(equalTo(Fixtures.rfc7541AppendixC_1_1_encoded))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.1.2") {
      val actual = encoderFunctions.rfc7541AppendixC_1_2
      assert(actual)(equalTo(Fixtures.rfc7541AppendixC_1_2_encoded))
    } @@ timeout(10.seconds),
    test("RFC 7541 Appendix C.1.3") {
      val actual = encoderFunctions.rfc7541AppendixC_1_3
      assert(actual)(equalTo(Fixtures.rfc7541AppendixC_1_3_encoded))
    } @@ timeout(10.seconds)
  )
}
