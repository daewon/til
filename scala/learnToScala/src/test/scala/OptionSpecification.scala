import io.daewon.util._
import org.specs2.mutable.Specification

import scala.collection.mutable.ListBuffer

class OptionSpecification extends Specification {
  "Option Specifications" >> {
    "Map" >> {
      "(_ * 2)" >> {
        val original = Some(1)
        original.map(_ * 2) === Some(2)
      }
    }

    "FlatMap" >> {
      "basic" >> {
        val original = Some(1)
        val flatted = original.flatMap { n =>
          Some(2)
        }

        flatted === Some(2)
      }

      "two depth" >> {
        val original = Some(2)
        val flatted = original.flatMap { n =>
          Some(Some(n))
        }

        flatted === Some(Some(2))
      }

      "for exp" >> {
        val original = Some(Some(2))

        val flatted = for {
          sm <- original
          n <- sm
        } yield n * 2

        flatted === Some(4)
      }
    }

    "IsDefined" >> {
      "int with BinaryAdd" >> {
        val original = None: Option[Int]
        original.isDefined === false
      }
    }

    "Foreach" >> {
      "for each element" >> {
        val items = new ListBuffer[Int]()
        Some(1).foreach((x) => items += x)

        items.length === 1
        items.head === Some(1).get
      }
    }

    "Find" >> {
      "find element" >> {
        val original = Some(3)
        original.find(x => x == 3) === Some(3)
        original.find(x => x == -1) === None
      }
    }

    "GetOrElse" >> {
      "Some" >> {
        val original = Some(3)
        original.getOrElse(throw new RuntimeException("cannot executed")) === 3
      }

      "None with fallback" >> {
        val original = None
        original.getOrElse(3) === 3
      }
    }

    "Apply" >> {
      "Null" >> {
        Option(null) === None
      }
    }

    "be associative" >> {
      val multiplier: Int => Option[Int] = v => Some(v * v)
      val divider: Int => Option[Int] = v => Some(v / 2)
      val original = Some(10)

      original.flatMap(multiplier).flatMap(divider) ===
        original.flatMap(v => multiplier(v).flatMap(divider))
    }

    "be left unit" >> {
      val multiplier: Int => Option[Int] = v => Some(v * v)
      val item = Some(10).flatMap(multiplier)

      item === multiplier(10)
    }

    "be identity" >> {
      val monad = Some(50).flatMap(v => Some(v))
      monad.flatMap(Option.apply) === monad
    }

    "be right unit" >> {
      val value = Some(50).flatMap(v => Some(v))
      value === Some(50)
    }
  }
}
