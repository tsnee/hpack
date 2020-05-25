package io.github.tsnee.hpack.codec.functional

import io.github.tsnee.hpack.codec.AbstractDecoderSpec

object ImmutableDecoderSpec extends AbstractDecoderSpec(
  "ImmutableDecoderSpec",
  new ImmutableDecoderBenchmark
)
