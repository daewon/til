// http://blog.jaceklaskowski.pl/2015/05/15/ad-hoc-polymorphism-in-scala-with-type-classes.html

sealed trait Exp

case class Num(value: Int) extends Exp

case class Plus(lhs: Exp, rhs: Exp) extends Exp

case class Minus(lhs: Exp, rhs: Exp) extends Exp

val exp = Minus(Num(10), Plus(Num(1), Num(2)))

object ExpressionEvaluator {
  def eval(exp: Exp): Int = exp match {
    case Num(n) => n
    case Plus(lhs, rhs) => eval(lhs) + eval(rhs)
    case Minus(lhs, rhs) => eval(lhs) - eval(rhs)
  }
}

ExpressionEvaluator.eval(exp)

// implicit

implicit class ExpOps(exp: Exp) {
  def value: Int = exp match {
    case n: Num => n.value
    case p: Plus => p.value
    case m: Minus => m.value
  }
}

implicit class PlusOps(exp: Plus) {
  def value = exp.lhs.value + exp.rhs.value
}

implicit class MinusOps(exp: Minus) {
  def value = exp.lhs.value - exp.rhs.value
}

exp.value

// subtype

trait Valueable {
  def value: Int
}

object Calculator {
  def calculate(v: Valueable) = v.value
}

sealed trait Exp2

case class Num2(value: Int) extends Exp2

case class Plus2(lhs: Exp2, rhs: Exp2) extends Exp2

case class Minus2(lhs: Exp2, rhs: Exp2) extends Exp2

implicit class ExpressionSubtype(exp: Exp2) extends Valueable {
  override def value: Int = exp match {
    case n: Num2 => n.value
    case p: Plus2 => p.value
    case m: Minus2 => m.value
  }
}

implicit class Plus2Ops(exp: Plus2) {
  def value = exp.lhs.value + exp.rhs.value
}

implicit class Minus2Ops(exp: Minus2) {
  def value = exp.lhs.value - exp.rhs.value
}

val exp2 = Minus2(Num2(10), Plus2(Num2(1), Num2(2)))
Calculator.calculate(exp2)


// typeclass

trait CanValue[T] {
  def value(t: T): Valueable
}

object CanValue {
  implicit val number2Value = new CanValue[Num2] {
    def value(n: Num2): Valueable = new Valueable {
      override def value: Int = n.value
    }
  }

  implicit val plus2Value = new CanValue[Plus2] {
    def value(p: Plus2): Valueable = new Valueable {
      override def value: Int = p.lhs.value + p.rhs.value
    }
  }

  implicit val minus2Value = new CanValue[Minus2] {
    def value(m: Minus2): Valueable = new Valueable {
      override def value: Int = m.lhs.value - m.rhs.value
    }
  }

  implicit val int2Value = new CanValue[Int] {
    def value(n: Int): Valueable = new Valueable {
      override def value: Int = n
    }
  }
}

object CalculatorEx {
  def calculate[T: CanValue](exp: T): Int =
    Calculator.calculate(implicitly[CanValue[T]].value(exp))

}

//CalculatorEx.calculate(exp2)
CalculatorEx.calculate(exp2)
CalculatorEx.calculate(10) // fn calculate can accept Int !


