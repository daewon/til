import scala.util.parsing.combinator._

object TestParser extends JavaTokenParsers {

  trait Expr

  case class Num(n: Int) extends Expr

  case class Add(a: Expr, b: Expr) extends Expr

  case class Mul(a: Expr, b: Expr) extends Expr

  type D = Expr

  def expr: Parser[D] = term ~ rep(plus) ^^ {
    case f ~ ls => ls.foldLeft(f.asInstanceOf[Expr]) { case (c, a) => Add(c, a) }
  }

  def plus = "+" ~> term

  def term: Parser[D] = factor ~ rep(times) ^^ {
    case f ~ ls => ls.foldLeft(f.asInstanceOf[Expr]) { case (c, a) => Mul(c, a) }
  }

  def times = "*" ~> factor

  def factor: Parser[D] = fpn | "(" ~> expr <~ ")"

  def fpn: Parser[D] = floatingPointNumber ^^ { case n => Num(n.toInt) }

  def parse(str: String) = parseAll(expr, str) match {
    case Success(result, next) => result
    case NoSuccess(result) => result
  }
}

TestParser.parse("1")
TestParser.parse("3 * 2 + 1")
TestParser.parse("3 + 2 * 1")

object ABParser extends JavaTokenParsers {

  trait P

  object A extends P

  object B extends P

  def a: Parser[P] = {
    println("a");
    "A".r ^^^ A
  }

  def b: Parser[P] = {
    println("b");
    "B".r ^^^ B
  }

  def expr = a | b

  def parse(str: String) = parseAll(expr, str) match {
    case Success(result, next) => result
    case NoSuccess(result) => result
  }
}

ABParser.parse("B") // first parse a, if it fails parse b

