package io.github.tsnee.hpack.table

import io.github.tsnee.hpack.HeaderField

private[table] abstract class IndexTable(backingStore: Vector[HeaderField]) {
  def lookup(idx: Int): Option[HeaderField] = backingStore.lift(idx - 1)

  val numEntries: Int = backingStore.length
}
