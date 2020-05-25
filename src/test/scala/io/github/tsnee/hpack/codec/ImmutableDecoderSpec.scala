package io.github.tsnee.hpack.codec

object ImmutableDecoderSpec extends AbstractDecoderSpec(
  "ImmutableDecoderSpec",
  new HpackImmutableDecoderBenchmark
)
