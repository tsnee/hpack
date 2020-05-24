package io.github.tsnee.hpack.codec

object VectorDecoderSpec extends AbstractDecoderSpec(
  "VectorDecoderSpec",
  new HpackVectorBenchmark
)
