package io.github.tsnee.hpack

trait HpackBenchmark[T] {
  def decodingAnEmptyHeaderBlockYieldsAnEmptyHeaderList(
    input: T
  ): Seq[HeaderField]
  def rfc7541AppendixC_1_1(input: T): Either[Error, (Int, Int)]
  def rfc7541AppendixC_1_2(input: T): Either[Error, (Int, Int)]
  def rfc7541AppendixC_1_3(input: T): Either[Error, (Int, Int)]
  def rfc7541AppendixC_2_1_in_one_chunk(input: T): Seq[HeaderField]
  def rfc7541AppendixC_2_1_in_two_chunks(input: T): Seq[HeaderField]
  def rfc7541AppendixC_2_2(input: T): Seq[HeaderField]
  def rfc7541AppendixC_2_3(input: T): Seq[HeaderField]
  def rfc7541AppendixC_2_4(input: T): Seq[HeaderField]
  def rfc7541AppendixC_3_1(input: T): Seq[HeaderField]
  def rfc7541AppendixC_3_2(input: T): Seq[HeaderField]
  def rfc7541AppendixC_3_3(input: T): Seq[HeaderField]
  def rfc7541AppendixC_4_1(input: T): Seq[HeaderField]
}
