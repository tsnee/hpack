package io.github.tsnee.hpack

object ChunkDecoderSpec extends AbstractDecoderSpec[ChunkInput](
  "ChunkDecoderSpec",
  new HpackChunkBenchmark,
  new ChunkInput
)
