package io.daewon.util

import org.specs2.mutable.Specification
import scala.collection.mutable.ListBuffer

import io.daewon.util._

class ListSpecification extends Specification {
  "List Specifications" >> {
    "Map" >> {
      "(_ * 2)" >> {
        val original = List(1, 2, 3)
        original.map(_ * 2) === List(2, 4, 6)
      }
    }

    "FlatMap" >> {
      "basic" >> {
        val original = List(1, 2, 3)
        val flatted = original.flatMap { n =>
          List(n * 2)
        }

        flatted === original.map(_ * 2)
      }

      "two depth" >> {
        val original = List(1, 2, 3)
        val flatted = original.flatMap { n =>
          List(List(n))
        }

        flatted === List(List(1), List(2), List(3))
      }

      "for exp" >> {
        val original = List(List(1), List(2), List(3))

        val flatted = for {
          ls <- original
          n <- ls
        } yield n * 2

        flatted === List(2, 4, 6)
      }
    }

    "Fold" >> {
      "int with BinaryAdd" >> {
        val original = List(1, 2, 3)
        val sum = original.fold(0) { (acc, curr) =>
          acc + curr
        }
        sum === 6
      }

      "string with BinaryAdd" >> {
        val original = List("A", "B", "C")
        val sum = original.fold("") { (acc, curr) =>
          acc + curr
        }
        sum === "ABC"

        "fold" >> {
          val original = List("A", "B", "C")
          val folded = original.fold("")(_ + _)
          folded === "ABC"
        }
      }

      "foldRight" >> {
        val original = List("A", "B", "C")
        val folded = original.foldRight("")(_ + _)
        folded === "ABC"
      }
    }

    "Reverse" >> {
      "basic reverse" >> {
        val original = List(1, 2, 3)
        original.reverse === List(3, 2, 1)
      }
    }

    "Filter" >> {
      "basic wiht predIsOdd" >> {
        val original = List(1, 2, 3)
        original.filter(_ % 2 != 0) === List(1, 3)
      }
    }

    "Size" >> {
      "size nonEmpty" >> {
        val original = List(1, 2, 3)
        original.size === 3
      }

      "size with cons List" >> {
        val original = List(1, 2, 3)
        val mapped = original.map(x => x * 2)
        (original ::: mapped).size === 6
      }

      "size Empty" >> {
        val original = List()
        original.size === 0
      }
    }

    "Cons" >> {
      "build lists with cons" in {
        List(1, 2, 3, 4) === 1 :: 2 :: 3 :: 4 :: Empty
      }
    }

    "Prepend" >> {
      "appending two lists" >> {
        val current = List(1, 2, 3, 4)
        val other = List(10, 11, 12, 13)

        (other ::: current) === List(10, 11, 12, 13, 1, 2, 3, 4)
        other.prepends(current) mustEqual List(1, 2, 3, 4, 10, 11, 12, 13)
      }
    }

    "Foreach" >> {
      "for each element" >> {
        val items = new ListBuffer[Int]()
        List(1, 2, 3).foreach((x) => items += x)

        items.length === List(1, 2, 3).size
        items.head === List(1, 2, 3).head
        items.tail.head === List(1, 2, 3).tail.head
        items.tail.tail.head === List(1, 2, 3).tail.tail.head
      }
    }

    "Find" >> {
      "find element" >> {
        val original = List(1, 2, 3, 4)
        original.find(x => x == 3) === Some(3)
        original.find(x => x == -1) === None
      }
    }
  }
}
