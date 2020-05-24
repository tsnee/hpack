package io.github.tsnee.hpack

object ChunkDecoderSpec extends AbstractDecoderSpec(
  "ChunkDecoderSpec",
  new HpackChunkBenchmark
)
