package io.github.tsnee.hpack.table

import io.github.tsnee.hpack.HeaderField
import zio.test.Assertion._
import zio.test._

object DynamicTableSpec extends DefaultRunnableSpec {
  override def spec = suite("DynamicTableSpec")(
    test("lookup(StaticTable.numEntries + 1) on empty table returns None") {
      val table = DynamicTable(1024)
      val actual = table.lookup(StaticTable.numEntries + 1)
      assert(actual)(equalTo(None))
    },
    test("lookup(-1) on non-empty table returns None") {
      val table = DynamicTable(1024).store(HeaderField("name", "value"), Indexing.With)
      val actual = table.lookup(-1)
      assert(actual)(equalTo(None))
    },
    test("lookup(0) on non-empty table returns None") {
      val table = DynamicTable(1024).store(HeaderField("name", "value"), Indexing.With)
      val actual = table.lookup(0)
      assert(actual)(equalTo(None))
    },
    test("lookup(StaticTable.numEntries + 1) on non-empty returns Some") {
      val headerField = HeaderField("name", "value")
      val table = DynamicTable(1024).store(headerField, Indexing.With)
      val actual = table.lookup(StaticTable.numEntries + 1)
      assert(actual)(equalTo(Some(headerField)))
    },
    test("lookup(StaticTable.numEntries + 2) on size-one table returns None") {
      val table = DynamicTable(1024).store(HeaderField("name", "value"), Indexing.With)
      val actual = table.lookup(StaticTable.numEntries + 2)
      assert(actual)(equalTo(None))
    },
    test("resize(0) on nonempty table returns empty table") {
      val table = DynamicTable(1024).store(HeaderField("name", "value"), Indexing.With)
      val resized = table.resize(0)
      val actual = resized.lookup(StaticTable.numEntries + 1)
      assert(actual)(equalTo(None))
    },
    test("resize(0) on nonempty table returns table of size 0") {
      val table = DynamicTable(1024).store(HeaderField("name", "value"), Indexing.With)
      val resized = table.resize(0)
      val actual = resized.size
      assert(actual)(equalTo(0))
    },
    test("resize(0) on nonempty table returns table with maxSize 0") {
      val table = DynamicTable(1024).store(HeaderField("name", "value"), Indexing.With)
      val resized = table.resize(0)
      val actual = (table.maxSize, resized.maxSize)
      assert(actual)(equalTo((1024, 0)))
    },
    test("store(anything) on maxSize 0 table returns empty table") {
      val table = DynamicTable(0).store(HeaderField("name", "value"), Indexing.With)
      val actual = table.lookup(StaticTable.numEntries + 1)
      assert(actual)(equalTo(None))
    },
    test("store(anything) on maxSize 0 table returns table with size 0") {
      val table = DynamicTable(0).store(HeaderField("name", "value"), Indexing.With)
      val actual = table.size
      assert(actual)(equalTo(0))
    },
    test("store(anything) on maxSize 0 table returns table with maxSize 0") {
      val table = DynamicTable(0).store(HeaderField("name", "value"), Indexing.With)
      val actual = table.maxSize
      assert(actual)(equalTo(0))
    },
    test("store(header) on a too-small table returns table with no entries") {
      val headerField = HeaderField("name", "value")
      val table = DynamicTable(headerField.size - 1).store(headerField, Indexing.With)
      val actual = table.lookup(StaticTable.numEntries + 1)
      assert(actual)(equalTo(None))
    },
    test("store(header) on right-sized table returns table with one entry") {
      val headerField = HeaderField("name", "value")
      val table = DynamicTable(headerField.size).store(headerField, Indexing.With)
      val actual = table.lookup(StaticTable.numEntries + 1)
      assert(actual)(equalTo(Some(headerField)))
    },
    test("store(two headers) on not-quite-big-enough table returns table with one entry") {
      val headerField1 = HeaderField("name1", "value1")
      val headerField2 = HeaderField("name2", "value2")
      val tooSmall = headerField1.size + headerField2.size - 1
      val table = DynamicTable(tooSmall)
        .store(headerField1, Indexing.With)
        .store(headerField2, Indexing.With)
      val actual = table.lookup(StaticTable.numEntries + 1)
      assert(actual)(equalTo(Some(headerField2)))
    },
    test("store(two headers) on right-sized table returns table with two entries") {
      val headerField1 = HeaderField("name1", "value1")
      val headerField2 = HeaderField("name2", "value2")
      val rightSize = headerField1.size + headerField2.size
      val table = DynamicTable(rightSize)
        .store(headerField1, Indexing.With)
        .store(headerField2, Indexing.With)
      val firstIdx = StaticTable.numEntries + 1
      val secondIdx = firstIdx + 1
      val actual = (table.lookup(firstIdx), table.lookup(secondIdx))
      assert(actual)(equalTo((Some(headerField2), Some(headerField1))))
    },
    test("store(two headers) on right-sized table resized by -1 returns table with one entry") {
      val headerField1 = HeaderField("name1", "value1")
      val headerField2 = HeaderField("name2", "value2")
      val rightSize = headerField1.size + headerField2.size
      val table = DynamicTable(rightSize)
        .store(headerField1, Indexing.With)
        .store(headerField2, Indexing.With)
        .resize(rightSize - 1)
      val actual = table.lookup(StaticTable.numEntries + 1)
      assert(actual)(equalTo(Some(headerField2)))
    },
    test("store(three headers) and retrieve them in order") {
      val headerField1 = HeaderField("name1", "value1")
      val headerField2 = HeaderField("name2", "value2")
      val headerField3 = HeaderField("name3", "value3")
      val table = DynamicTable(1024)
        .store(headerField1, Indexing.With)
        .store(headerField2, Indexing.With)
        .store(headerField3, Indexing.With)
      val first = table.lookup(StaticTable.numEntries + 1)
      val second = table.lookup(StaticTable.numEntries + 2)
      val third = table.lookup(StaticTable.numEntries + 3)
      assert(first)(equalTo(Some(headerField3))) &&
      assert(second)(equalTo(Some(headerField2))) &&
      assert(third)(equalTo(Some(headerField1)))
    }
  )
}
