import io.daewon.SimpleCalc._
import io.daewon._
import org.specs2.mutable.Specification

class SimpleCalcSpec extends Specification {
  "SimpleCalc Specifications" >> {

    "const" >> {
      SimpleCalc.parse("1") === EConst(1)
    }

    "add" >> {
      SimpleCalc.parse("1 + 1") === EAdd(EConst(1.0), EConst(1.0))
      SimpleCalc.parse("1 - 1") === ESub(EConst(1.0), EConst(1.0))
      SimpleCalc.parse("1 - 1 - 3") === ESub(ESub(EConst(1.0), EConst(1.0)), EConst(3.0))
    }

    "multiply" >> {
      SimpleCalc.parse("1 * 1") === EMul(EConst(1.0), EConst(1.0))
      SimpleCalc.parse("1 - 1 * 3") ===
        ESub(EConst(1.0), EMul(EConst(1.0), EConst(3.0)))
    }

    "parens" >> {
      SimpleCalc.parse("(1 - 1) * 3") ===
        EMul(ESub(EConst(1.0), EConst(1.0)), EConst(3.0))
    }

    "unary op" >> {
      SimpleCalc.parse("3 * -(2+2)") ===
        EMul(EConst(3.0), EUMinus(EAdd(EConst(2.0), EConst(2.0))))
    }
  }
}
