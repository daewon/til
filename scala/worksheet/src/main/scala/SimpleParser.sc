package scala.util.parsing.combinator._

object TestParser extends JavaTokenParsers {
  trait Exp
  case class Num(n: Int) extends Exp
  case class Add(a: Exp, b: Exp) extends Exp
