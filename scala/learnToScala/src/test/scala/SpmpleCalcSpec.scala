import io.daewon.SimpleCalc._
import io.daewon._
import org.specs2.mutable.Specification

class SimpleCalcSpec extends Specification {
  "SimpleCalc Specifications" >> {

    "1" >> {
      SimpleCalc.parse("1") === EConst(1)
    }

    "1 + 1" >> {
      SimpleCalc.parse("1 + 1") === EAdd(EConst(1.0), EConst(1.0))
      SimpleCalc.parse("1 - 1") === ESub(EConst(1.0), EConst(1.0))
      SimpleCalc.parse("1 - 1 - 3") === ESub(ESub(EConst(1.0), EConst(1.0)), EConst(3.0))
    }
  }
}
