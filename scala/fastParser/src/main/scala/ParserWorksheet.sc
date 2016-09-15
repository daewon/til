import fastparse.all._

trait Exp

case class Number(n: Int) extends Exp {
  override def toString = n.toString
}

case class Plus(lhs: Exp, rhs: Exp) extends Exp {
  override def toString = s"(+ $lhs $rhs)"
}

case class Minus(lhs: Exp, rhs: Exp) extends Exp {
  override def toString = s"(- $lhs $rhs)"
}

case class Multiply(lhs: Exp, rhs: Exp) extends Exp {
  override def toString = s"(* $lhs $rhs)"
}

val ws = P(" ".rep(0))

val one = P("one")
val two = P("two")
val three = P("three")
val four = P("four")

val number = P(CharIn('0' to '9')).!.map(n => Number(n.toInt))
val plus = P("+")
val minus = P("-")
val times = P("*")

val factor = number

def makeTree(a: Exp, b: Exp)(f: (Exp, Exp) => Exp): Exp = f(a, b)

val timesOp: P[Exp] = P(factor ~ (ws ~ times ~ ws ~ factor).rep).map {
  case (e, exprs) =>
    Multiply(e, exprs.reduceLeft[Exp] { case (acc, curr) =>
      makeTree(acc, curr)(Multiply)
    })
}

val parser = timesOp

parser.parse("1 * 2 * 3")


