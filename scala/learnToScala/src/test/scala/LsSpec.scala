import org.specs2.mutable.Specification

class LsSpecification extends Specification {
  "Ls Specifications" >> {
    "map test" >> {
      "(_ * 2)" >> {
        val original = Ls(1, 2, 3)
        original.map(_ * 2) mustEqual Ls(2, 4, 6)
      }
    }

    "fold test" >> {
      "Int with BinaryAdd" >> {
        val original = Ls(1, 2, 3)
        val sum = original.fold(0) { (acc, curr) =>
          acc + curr
        }
        sum mustEqual 6
      }

      "String with BinaryAdd" >> {
        val original = Ls("A", "B", "C")
        val sum = original.fold("") { (acc, curr) =>
          acc + curr
        }
        sum mustEqual "ABC"

        "fold" >> {
          val original = Ls("A", "B", "C")
          val folded = original.fold("")(_ + _)
          folded mustEqual "ABC"
        }
      }

      "foldRight" >> {
        val original = Ls("A", "B", "C")
        val folded = original.foldRight("")(_ + _)
        folded mustEqual "ABC"
      }
    }

    "Reverse" >> {
      "basic reverse" >> {
        val original = Ls(1, 2, 3)
        original.reverse mustEqual Ls(3, 2, 1)
      }
    }

    "Filter" >> {
      "basic wiht predIsOdd" >> {
        val original = Ls(1, 2, 3)
        original.filter(_ % 2 != 0) mustEqual Ls(1, 3)
      }
    }

    "Size" >> {
      "size nonEmpty" >> {
        val original = Ls(1, 2, 3)
        original.size mustEqual 3
      }

      "size with cons ls" >> {
        //        val original = Ls(1, 2, 3)
        //        val newLs = Ls(1, 2, 3)
        //        original.size mustEqual 3
      }

      "size Empty" >> {
        val original = Ls()
        original.size mustEqual 0
      }
    }

    "Cons" >> {
      "build lists with cons" in {
        Ls(1, 2, 3, 4) == 1 :: 2 :: 3 :: 4 :: Empty
      }
    }
    }
  }
