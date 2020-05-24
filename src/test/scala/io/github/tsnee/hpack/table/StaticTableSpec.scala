package io.github.tsnee.hpack.table

import io.github.tsnee.hpack.HeaderField
import zio.test.Assertion._
import zio.test._

object StaticTableSpec extends DefaultRunnableSpec {
  override def spec = suite("StaticTableSpec")(
    test("lookup(-1) on static table returns None") {
      val actual = StaticTable.lookup(-1)
      assert(actual)(equalTo(None))
    },
    test("lookup(0) on static table returns None") {
      val actual = StaticTable.lookup(0)
      assert(actual)(equalTo(None))
    },
    test("lookup(1) on static returns Some") {
      val actual = StaticTable.lookup(1)
      assert(actual)(equalTo(Some(HeaderField(":authority", ""))))
    },
    test("lookup(2) on static table returns Some") {
      val actual = StaticTable.lookup(2)
      assert(actual)(equalTo(Some(HeaderField(":method", "GET"))))
    },
    test("lookup(62) on static table returns None") {
      val actual = StaticTable.lookup(62)
      assert(actual)(equalTo(None))
    },
    test("numEntries on static table returns 61") {
      val actual = StaticTable.numEntries
      assert(actual)(equalTo(61))
    }
  )
}
