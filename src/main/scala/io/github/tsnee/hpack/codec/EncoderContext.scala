package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack.table.{DynamicTable, Indexing}
import io.github.tsnee.hpack.codec.functional.ImmutableEncoderContext
import zio.Chunk

trait EncoderContext {
  def headerBlock: (Chunk[Byte], EncoderContext)
  def index(fieldMapping: Map[String, Indexing]): EncoderContext
  def compressByDefault(b: Boolean): EncoderContext
  def compress(fields: Set[String]): EncoderContext
  def doNotCompress(fields: Set[String]): EncoderContext
}

object EncoderContext {
  def default(dynamicTableSize: Int): EncoderContext =
    ImmutableEncoderContext(DynamicTable(dynamicTableSize))
}
