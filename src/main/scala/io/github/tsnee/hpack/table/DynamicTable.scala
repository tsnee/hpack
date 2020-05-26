package io.github.tsnee.hpack.table

import scala.annotation.tailrec
import io.github.tsnee.hpack.HeaderField
import zio.Chunk

class DynamicTable private (
  val maxSize: Int,
  backingStore: Chunk[HeaderField],
  val size: Int
) extends IndexTable(backingStore) {

  assert(maxSize >= 0 && size >= 0)

  override def lookup(idx: Int): Option[HeaderField] = {
    val adjustedIdx = idx - StaticTable.numEntries
    if (adjustedIdx < 1)
      StaticTable.lookup(idx)
    else
      backingStore.lift(adjustedIdx - 1)
  }

  override def find(hf: HeaderField): Match = StaticTable.find(hf) match {
    case m: Match.Full => m
    case p: Match.Partial => super.find(hf) match {
      case Match.Full(idx) => Match.Full(StaticTable.numEntries + idx)
      case _ => p
    }
    case Match.NotFound => super.find(hf) match {
      case Match.Full(idx) => Match.Full(StaticTable.numEntries + idx)
      case Match.Partial(idx) => Match.Partial(StaticTable.numEntries + idx)
      case Match.NotFound => Match.NotFound
    }
  }

  def store(headerField: HeaderField, indexing: Indexing): DynamicTable =
    indexing match {
      case Indexing.With => shrink(
        maxSize,
        backingStore.prepended(headerField),
        size + headerField.size
      )
      case _ => this
    }

  def resize(newMaxSize: Int): DynamicTable = {
    require(newMaxSize >= 0)
    if (newMaxSize == 0)
      new DynamicTable(newMaxSize, Chunk.empty, 0)
    else
      shrink(newMaxSize, backingStore, size)
  }

  @tailrec
  private def shrink(newMaxSize: Int, fields: Chunk[HeaderField], size: Int): DynamicTable = {
    assert(newMaxSize >= 0 && size >= 0)
    if (size <= newMaxSize)
      new DynamicTable(newMaxSize, fields, size)
    else {
      val headerField = fields.last
      val smaller = fields.init
      shrink(newMaxSize, smaller, size - headerField.size)
    }
  }

  override lazy val toString: String =
    s"""Current size: $size
       |Max size: $maxSize
       |Table: """.stripMargin + backingStore.mkString("\t", "\n\t", "")
}

object DynamicTable {
  def apply(maxSize: Int): DynamicTable = {
    require(maxSize >= 0)
    new DynamicTable(maxSize, Chunk.empty, 0)
  }
}
