package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack._
import zio.Chunk

trait DecoderBenchmark {
  def decodingAnEmptyHeaderBlockYieldsAnEmptyHeaderList: Seq[HeaderField]
  def rfc7541AppendixC_1_1: Either[DecoderError, (Int, Int)]
  def rfc7541AppendixC_1_2: Either[DecoderError, (Int, Int)]
  def rfc7541AppendixC_1_3: Either[DecoderError, (Int, Int)]
  def rfc7541AppendixC_2_1_in_one_chunk: Seq[HeaderField]
  def rfc7541AppendixC_2_1_in_two_chunks: Seq[HeaderField]
  def rfc7541AppendixC_2_2: Seq[HeaderField]
  def rfc7541AppendixC_2_3: Seq[HeaderField]
  def rfc7541AppendixC_2_4: Seq[HeaderField]
  def rfc7541AppendixC_3_1: Seq[HeaderField]
  def rfc7541AppendixC_3_2: Seq[HeaderField]
  def rfc7541AppendixC_3_3: Seq[HeaderField]
  def rfc7541AppendixC_4_1: Seq[HeaderField]
  def rfc7541AppendixC_4_2: Seq[HeaderField]
  def rfc7541AppendixC_4_3: Seq[HeaderField]
}
