package io.github.tsnee.hpack.codec.optimized

import io.github.tsnee.hpack.codec.AbstractDecoderSpec
import io.github.tsnee.hpack.codec.optimized.MutableDecoderBenchmark

object MutableDecoderSpec extends AbstractDecoderSpec(
  "MutableDecoderSpec",
  new MutableDecoderBenchmark
)
