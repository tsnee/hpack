package io.github.tsnee.hpack.codec

object ChunkDecoderSpec extends AbstractDecoderSpec(
  "ChunkDecoderSpec",
  new HpackChunkBenchmark
)
