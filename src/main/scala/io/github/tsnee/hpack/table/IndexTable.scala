package io.github.tsnee.hpack.table

import scala.annotation.tailrec
import io.github.tsnee.hpack.HeaderField
import zio.Chunk

private[table] abstract class IndexTable(backingStore: Chunk[HeaderField]) {
  val numEntries: Int = backingStore.length

  def lookup(idx: Int): Option[HeaderField] = backingStore.lift(idx - 1)

  def find(hf: HeaderField): Match =
    findRecursive(hf, Match.NotFound, 1)

  @tailrec
  private def findRecursive(field: HeaderField, prev: Match, idx: Int): Match = {
    if (backingStore.lengthCompare(idx) <= 0) {
      val current = backingStore(idx - 1)
      if (current.name == field.name)
        if (current.value == field.value)
          Match.Full(idx)
        else
          findRecursive(field, Match.Partial(idx), idx + 1)
      else
        findRecursive(field, prev, idx + 1)
    }
    else
      prev
  }
}
