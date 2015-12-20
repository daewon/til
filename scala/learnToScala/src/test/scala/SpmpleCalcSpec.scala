import io.daewon.til.SimpleCalc.Num
import io.daewon.til._
import org.specs2.mutable.Specification

class SimpleCalcSpec extends Specification {
  "SimpleCalc Specifications" >> {

    "1" >> {
      SimpleCalc.parse("1") === Num(1)
    }

    "1 + 1 + 1" >> {
      println(SimpleCalc.parse("1 + 1 + 1"))
      SimpleCalc.parse("1 + 1 + 1") === Num(3)
    }
  }
}
