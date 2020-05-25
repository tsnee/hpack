package io.github.tsnee.hpack.codec

object MutableDecoderSpec extends AbstractDecoderSpec(
  "MutableDecoderSpec",
  new HpackMutableDecoderBenchmark
)
