import org.specs2.mutable._

class LsSpecification extends Specification {
  "Ls Specifications" >> {
    "map test" >> {
      "(_ * 2)" >> {
        val original = Ls(1, 2, 3)
        original.map(_ * 2) mustEqual Ls(2, 4, 6)
      }
    }

    "fold test" >> {
      "Int with _ + _" >> {
        val original = Ls(1, 2, 3)
        val sum = original.fold(0) { (acc, curr) =>
          acc + curr
        }
        sum mustEqual 6
      }
      "String with _ + _" >> {
        val original = Ls("A", "B", "C")
        val sum = original.fold("") { (acc, curr) =>
          acc + curr
        }
        sum mustEqual "ABC"
      }
    }
  }
}
