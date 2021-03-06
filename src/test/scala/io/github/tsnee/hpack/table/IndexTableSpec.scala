package io.github.tsnee.hpack.table

import io.github.tsnee.hpack.HeaderField
import zio.Chunk
import zio.test.Assertion._
import zio.test._

object IndexTableSpec extends DefaultRunnableSpec {
  override def spec = suite("IndexTableSpec")(
    test("lookup(1) on empty table returns None") {
      val table = new IndexTable(Chunk.empty) {}
      val actual = table.lookup(1)
      assert(actual)(equalTo(None))
    },
    test("numEntries on empty table returns 0") {
      val table = new IndexTable(Chunk.empty) {}
      val actual = table.numEntries
      assert(actual)(equalTo(0))
    },
    test("lookup(-1) on non-empty table returns None") {
      val headerField = HeaderField("name", "value")
      val fields = Chunk(headerField)
      val table = new IndexTable(fields) {}
      val actual = table.lookup(-1)
      assert(actual)(equalTo(None))
    },
    test("lookup(0) on non-empty table returns None") {
      val headerField = HeaderField("name", "value")
      val fields = Chunk(headerField)
      val table = new IndexTable(fields) {}
      val actual = table.lookup(0)
      assert(actual)(equalTo(None))
    },
    test("lookup(1) on non-empty returns Some") {
      val headerField = HeaderField("name", "value")
      val fields = Chunk(headerField)
      val table = new IndexTable(fields) {}
      val actual = table.lookup(1)
      assert(actual)(equalTo(Some(headerField)))
    },
    test("numEntries on size-one table returns 1") {
      val headerField = HeaderField("name", "value")
      val fields = Chunk(headerField)
      val table = new IndexTable(fields) {}
      val actual = table.numEntries
      assert(actual)(equalTo(1))
    },
    test("lookup(2) on size-one table returns None") {
      val headerField = HeaderField("name", "value")
      val fields = Chunk(headerField)
      val table = new IndexTable(fields) {}
      val actual = table.lookup(2)
      assert(actual)(equalTo(None))
    }
  )
}
