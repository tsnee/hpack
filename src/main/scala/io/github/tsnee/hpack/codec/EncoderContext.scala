package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack.stringToByteChunk
import io.github.tsnee.hpack.table.{DynamicTable, Indexing}
import zio.Chunk

trait EncoderContext {
  def index(fieldMapping: Map[String, Indexing]): EncoderContext
  def compressByDefault(b: Boolean): EncoderContext
  def compress(fields: Set[String]): EncoderContext
  def doNotCompress(fields: Set[String]): EncoderContext
}

object EncoderContext {
  def default(dynamicTableSize: Int): EncoderContext =
    ImmutableEncoderContext(DynamicTable(dynamicTableSize))
}

private[codec] case class ImmutableEncoderContext(
  table: DynamicTable,
  indexed: Map[Chunk[Byte], Indexing] = Map.empty.withDefaultValue(Indexing.With),
  compressedByDefault: Boolean = true,
  compressed: Set[Chunk[Byte]] = Set.empty,
  notCompressed: Set[Chunk[Byte]] = Set.empty,
  headerBlock: Chunk[Byte] = Chunk.empty
) extends EncoderContext {
  override def index(fieldMapping: Map[String, Indexing]): EncoderContext =
    copy(indexed = fieldMapping.map {
      case (k, v) => stringToByteChunk(k) -> v
    })

  override def compressByDefault(b: Boolean): EncoderContext =
    copy(compressedByDefault = b)

  override def compress(fields: Set[String]): EncoderContext =
    copy(compressed = fields.map(s => stringToByteChunk(s)))

  override def doNotCompress(fields: Set[String]): EncoderContext =
    copy(notCompressed = fields.map(s => stringToByteChunk(s)))
}
