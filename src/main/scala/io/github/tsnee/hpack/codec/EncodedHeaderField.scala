package io.github.tsnee.hpack.codec

import io.github.tsnee.hpack._
import io.github.tsnee.hpack.table.Indexing
import zio.Chunk

sealed trait EncodedHeaderField {
  def header(indexing: Indexing): Byte
}

object IndexedHeaderField extends EncodedHeaderField {
  override def header(irrelevant: Indexing): Byte = 0x80
}

object LiteralHeaderField extends EncodedHeaderField {
  override def header(indexing: Indexing): Byte = indexing match {
    case Indexing.With => 0x40
    case Indexing.Without => 0x00
    case Indexing.Never => 0x10
  }
}