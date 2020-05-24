package io.github.tsnee.hpack

object VectorDecoderSpec extends AbstractDecoderSpec(
  "VectorDecoderSpec",
  new HpackVectorBenchmark
)
