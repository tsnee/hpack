package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack._
import io.github.tsnee.hpack.table.Indexing
import zio.Chunk

sealed trait EncodedHeaderField {
  def header(indexing: Indexing): (Byte, Int)
}

object IndexedHeaderField extends EncodedHeaderField {
  override def header(irrelevant: Indexing): (Byte, Int) = (0x80, 1)
}

object LiteralHeaderField extends EncodedHeaderField {
  override def header(indexing: Indexing): (Byte, Int) = indexing match {
    case Indexing.With => (0x40, 2)
    case Indexing.Without => (0x00, 4)
    case Indexing.Never => (0x10, 4)
  }
}