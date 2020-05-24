package io.github.tsnee.hpack

object VectorDecoderSpec extends AbstractDecoderSpec[VectorInput](
  "VectorDecoderSpec",
  new HpackVectorBenchmark,
  new VectorInput
)
