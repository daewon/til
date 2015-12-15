import org.specs2.mutable.Specification

import scala.collection.mutable.ListBuffer

class LsSpecification extends Specification {
  "Ls Specifications" >> {
    "map" >> {
      "(_ * 2)" >> {
        val original = Ls(1, 2, 3)
        original.map(_ * 2) === Ls(2, 4, 6)
      }
    }

    "flatMap" >> {
      "basic" >> {
        val original = Ls(1, 2, 3)
        val flatted = original.flatMap { (n: Int) =>
          Ls(n * 2)
        }

        flatted === original.map(_ * 2)
      }

      "two depth" >> {
        val original = Ls(1, 2, 3)
        val flatted = original.flatMap { n =>
          Ls(Ls(n))
        }

        flatted === Ls(Ls(1), Ls(2), Ls(3))
      }

      "for exp" >> {
        val original = Ls(Ls(1), Ls(2), Ls(3))

        val flatted = for {
          ls <- original
          n <- ls
        } yield n * 2

        flatted === Ls(2, 4, 6)
      }
    }

    "fold" >> {
      "Int with BinaryAdd" >> {
        val original = Ls(1, 2, 3)
        val sum = original.fold(0) { (acc, curr) =>
          acc + curr
        }
        sum === 6
      }

      "String with BinaryAdd" >> {
        val original = Ls("A", "B", "C")
        val sum = original.fold("") { (acc, curr) =>
          acc + curr
        }
        sum === "ABC"

        "fold" >> {
          val original = Ls("A", "B", "C")
          val folded = original.fold("")(_ + _)
          folded === "ABC"
        }
      }

      "foldRight" >> {
        val original = Ls("A", "B", "C")
        val folded = original.foldRight("")(_ + _)
        folded === "ABC"
      }
    }

    "Reverse" >> {
      "basic reverse" >> {
        val original = Ls(1, 2, 3)
        original.reverse === Ls(3, 2, 1)
      }
    }

    "Filter" >> {
      "basic wiht predIsOdd" >> {
        val original = Ls(1, 2, 3)
        original.filter(_ % 2 != 0) === Ls(1, 3)
      }
    }

    "Size" >> {
      "size nonEmpty" >> {
        val original = Ls(1, 2, 3)
        original.size === 3
      }

      "size with cons ls" >> {
        val original = Ls(1, 2, 3)
        val mapped = original.map(x => x * 2)
        (original ::: mapped).size === 6
      }

      "size Empty" >> {
        val original = Ls()
        original.size === 0
      }
    }

    "Cons" >> {
      "build lists with cons" in {
        Ls(1, 2, 3, 4) === 1 :: 2 :: 3 :: 4 :: Empty
      }
    }

    "appending two lists" in {
      val current = Ls(1, 2, 3, 4)
      val other = Ls(10, 11, 12, 13)

      (other ::: current) === Ls(10, 11, 12, 13, 1, 2, 3, 4)
      other.prepends(current) mustEqual Ls(1, 2, 3, 4, 10, 11, 12, 13)
    }

    "foreach implementation" in {
      val items = new ListBuffer[Int]()

      Ls(1, 2, 3, 4).foreach((x) => items += x)

      items === List(1, 2, 3, 4)
    }
  }
}
