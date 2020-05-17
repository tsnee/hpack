package io.github.tsnee.hpack

import scala.collection.immutable.Queue

private[hpack] abstract class IndexTable(backingStore: Queue[HeaderField]) {
  def lookup(idx: Int): Option[HeaderField] = backingStore.lift(idx - 1)

  val numEntries: Int = backingStore.length
}
