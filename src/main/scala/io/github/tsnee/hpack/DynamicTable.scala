package io.github.tsnee.hpack

import scala.annotation.tailrec
import scala.collection.immutable.Queue

private[hpack] class DynamicTable private (
  val maxSize: Int,
  backingStore: Queue[HeaderField],
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

  def store(headerField: HeaderField): DynamicTable =
    headerField.indexing match {
      case Indexing.With =>
        shrink(maxSize, backingStore.enqueue(headerField), size + headerField.size)
      case _ => this
    }

  def resize(newMaxSize: Int): DynamicTable = {
    require(newMaxSize >= 0)
    if (newMaxSize == 0)
      new DynamicTable(newMaxSize, Queue.empty, 0)
    else
      shrink(newMaxSize, backingStore, size)
  }

  @tailrec
  private def shrink(newMaxSize: Int, q: Queue[HeaderField], qSize: Int): DynamicTable = {
    assert(newMaxSize >= 0 && qSize >= 0)
    if (qSize <= newMaxSize)
      new DynamicTable(newMaxSize, q, qSize)
    else {
      val (headerField, smallerQ) = q.dequeue
      shrink(newMaxSize, smallerQ, qSize - headerField.size)
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
    new DynamicTable(maxSize, Queue.empty, 0)
  }
}
