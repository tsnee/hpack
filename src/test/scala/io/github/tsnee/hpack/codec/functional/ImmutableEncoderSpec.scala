package io.github.tsnee.hpack.codec.functional

import io.github.tsnee.hpack.codec.AbstractEncoderSpec

object ImmutableEncoderSpec extends AbstractEncoderSpec(
  "ImmutableEncoderSpec",
  new ImmutableEncoderBenchmark
)
