package io.daewon

import scala.util.parsing.combinator.syntactical._

object SimpleCalc {

  sealed abstract class Expr {
    def eval(): Double
  }

  case class EConst(value: Double) extends Expr {
    def eval(): Double = value
  }

  case class EAdd(left: Expr, right: Expr) extends Expr {
    def eval(): Double = left.eval + right.eval
  }

  case class ESub(left: Expr, right: Expr) extends Expr {
    def eval(): Double = left.eval - right.eval
  }

  case class ESum(a: List[Expr]) extends Expr {
    def eval(): Double = a.foldLeft(0.0)(_ + _.eval)
  }

  case class EMul(left: Expr, right: Expr) extends Expr {
    def eval(): Double = left.eval * right.eval
  }

  case class EDiv(left: Expr, right: Expr) extends Expr {
    def eval(): Double = left.eval / right.eval
  }

  case class EUMinus(e: Expr) extends Expr {
    def eval(): Double = -e.eval
  }

  object ExprParser extends StandardTokenParsers {
    lexical.delimiters ++= Set("+", "-", "*", "/", "(", ")")

    def value = numericLit ^^ { s => EConst(s.toDouble) }

    def unaryMinus: Parser[EUMinus] = "-" ~> term ^^ {
      EUMinus(_)
    }

    def sum = product * (
      "+" ^^^ { (a: Expr, b: Expr) => EAdd(a, b) } |
        "-" ^^^ { (a: Expr, b: Expr) => ESub(a, b) }
      )

    def product = term * (
      "*" ^^^ { (a: Expr, b: Expr) => EMul(a, b) } |
        "/" ^^^ { (a: Expr, b: Expr) => EDiv(a, b) }
      )

    def parens = "(" ~> expr <~ ")"

    def term: Parser[Expr] = value | parens | unaryMinus

    def expr = sum | term

    def parse(s: String) = {
      val tokens = new lexical.Scanner(s)
      phrase(expr)(tokens)
    }

    def parse2(s: String) = {
      val tokens = new lexical.Scanner(s)
      phrase(expr)(tokens)
    }

    def apply(s: String): Expr = parse(s) match {
      case Success(tree, _) => tree
      case e: NoSuccess => throw new IllegalArgumentException("Bad syntax: " + s)
    }

    //Simplify testing
    def test(exprstr: String) = {
      parse(exprstr) match {
        case Success(tree, _) =>
          println("Tree: " + tree)
          val v = tree.eval()
          println("Eval: " + v)
        case e: NoSuccess => Console.err.println(e)
      }
    }
  }

  def parse(str: String) = ExprParser(str)
}


