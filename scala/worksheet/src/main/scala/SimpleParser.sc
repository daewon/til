import scala.util.parsing.combinator._

object TestParser extends JavaTokenParsers {

  trait Expr

  case class Num(n: Int) extends Expr

  case class Add(a: Expr, b: Expr) extends Expr

  case class Multi(a: Expr, b: Expr) extends Expr

  type D = Expr

  def expr = term ~ rep(add) ^^ {
    case t ~ ls => ls.foldLeft(t: Expr) { (acc, curr) => Add(acc, curr) }
  }

  def term = factor ~ rep(multi) ^^ {
    case f ~ ls => ls.foldLeft(f: Expr) { (acc, curr) => Multi(acc, curr) }
  }

  def multi = "*" ~> factor

  def add = "+" ~> term

  def factor: Parser[Expr] = num | "(" ~> expr <~ ")"

  def num = wholeNumber ^^ { n => Num(n.toInt) }

  def parse(str: String) = parseAll(expr, str) match {
    case Success(result, next) => result
    case result: NoSuccess => result
  }
}

TestParser.parse("1")
TestParser.parse("1 + 2")
TestParser.parse("1 * 2")
TestParser.parse("1 + 2 * 3")
TestParser.parse("1 * 2 + 3")
TestParser.parse("1 * (2 + 3)")
TestParser.parse("1 + 2 * (3 + 4)")
