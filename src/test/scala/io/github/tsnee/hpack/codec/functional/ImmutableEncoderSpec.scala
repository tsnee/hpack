package io.github.tsnee.hpack.codec.functional

import io.github.tsnee.hpack.codec.AbstractEncoderSpec
import io.github.tsnee.hpack.codec.functional.ImmutableEncoderBenchmark

object ImmutableEncoderSpec extends AbstractEncoderSpec(
  "ImmutableEncoderSpec",
  new ImmutableEncoderBenchmark
)
