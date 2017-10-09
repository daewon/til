package io.daewon.util

import utest._

object LruTests extends TestSuite {
  val tests = Tests {
    "simple case" - {
      val cache = new SimpleLRUCache[Int, Int](3)

      for (i <- 0 to 10) cache.set(i, i)

      assert(cache.size == 3)

      assert(cache.list.head.item.key == 10)
      assert(cache.list.head.item.value == 10)

      assert(cache.list.last.item.key == 8)
      assert(cache.list.last.item.value == 8)
    }

    "simple case 2" - {
      val cache = new SimpleLRUCache[Int, Int](2)

      for (i <- 0 to 10) cache.set(i, i)

      assert(cache.size == 2)

      assert(cache.list.head.item.key == 10)
      assert(cache.list.head.item.value == 10)

      assert(cache.list.last.item.key == 9)
      assert(cache.list.last.item.value == 9)
    }

    "simple case 3" - {
      val cache = new SimpleLRUCache[Int, Int](3)

      for (i <- 0 to 10) cache.set(i, i)

      assert(cache.size == 3)

      assert(cache.list.head.item.key == 10)
      assert(cache.list.head.item.value == 10)

      assert(cache.list.last.item.key == 8)
      assert(cache.list.last.item.value == 8)
    }

    "simple case with read" - {
      val cache = new SimpleLRUCache[Int, Int](3)

      for (i <- 0 to 10) cache.set(i, i)

      assert(cache.size == 3)

      assert(cache.list.head.item.key == 10)
      assert(cache.list.head.item.value == 10)

      assert(cache.list.last.item.key == 8)
      assert(cache.list.last.item.value == 8)

      cache.get(9) // touch

      assert(cache.list.head.item.key == 9)
      assert(cache.list.head.item.value == 9)

      assert(cache.list.last.item.key == 8)
      assert(cache.list.last.item.value == 8)
    }

    "simple case with read 2" - {
      val cache = new SimpleLRUCache[Int, Int](3)

      for (i <- 0 to 10) cache.set(i, i)

      assert(cache.size == 3)
      // remains 10, 9, 8

      // touch reverse order
      cache.get(8)
      cache.get(9)
      cache.get(10)

      assert(cache.list.head.item.key == 10)
      assert(cache.list.head.item.value == 10)

      assert(cache.list.last.item.key == 8)
      assert(cache.list.last.item.value == 8)
    }

    "simple case with read 3" - {
      val cache = new SimpleLRUCache[Int, Int](3)

      for (i <- 0 to 10) cache.set(i, i)

      assert(cache.size == 3)
      // remains 10, 9, 8

      // touch asc order
      cache.get(10)
      cache.get(9)
      cache.get(8)

      assert(cache.list.head.item.key == 8)
      assert(cache.list.head.item.value == 8)

      assert(cache.list.last.item.key == 10)
      assert(cache.list.last.item.value == 10)
    }

    "simple case with read and write" - {
      val cache = new SimpleLRUCache[Int, Int](3)

      for (i <- 0 to 10) cache.set(i, i)

      assert(cache.size == 3)
      // remains 10, 9, 8

      // touch asc order
      cache.get(10)
      cache.get(9)
      cache.get(8)
      cache.get(9)
      cache.get(10)

      assert(cache.list.head.item.key == 10)
      assert(cache.list.head.item.value == 10)

      assert(cache.list.last.item.key == 8)
      assert(cache.list.last.item.value == 8)

      cache.set(9, 90)
      cache.set(9, 91)
      cache.set(9, 92)

      assert(cache.list.head.item.key == 9)
      assert(cache.list.head.item.value == 92)

      assert(cache.list.last.item.key == 8)
      assert(cache.list.last.item.value == 8)

      cache.set(100, 100)
      cache.set(200, 200)

      assert(cache.list.head.item.key == 200)
      assert(cache.list.head.item.value == 200)

      assert(cache.list.last.item.key == 9)
      assert(cache.list.last.item.value == 92)
    }
  }
}
