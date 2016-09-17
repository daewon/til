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

val ws = P(" ".rep)

val one = P("one")
val two = P("two")
val three = P("three")
val four = P("four")

val number = P(CharIn('0' to '9')).!.map(n => Number(n.toInt))
val plus = P("+")
val minus = P("-")
val times = P("*")

lazy val parened = P("(" ~ ws ~ expr ~ ws ~ ")")

lazy val factor = P(number | parened)

lazy val timesOp: P[Exp] = P(factor ~ (ws ~ times ~ ws ~ factor).rep).map {
  case (e, expr) =>
    expr.foldLeft(e: Exp) { case (acc, curr) => Multiply(acc, curr) }
}

lazy val plusOp: P[Exp] = P(timesOp ~ (ws ~ plus ~ ws ~ timesOp).rep).map {
  case (e, expr) =>
    expr.foldLeft(e) { case (acc, curr) => Plus(acc, curr) }
}

lazy val expr = plusOp

expr.parse("1 * 2 + (3 + 4)")



