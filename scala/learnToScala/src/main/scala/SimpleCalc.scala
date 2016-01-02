package io.daewon

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/**
  http://www.henkelmann.eu/2011/1/13/an_introduction_to_scala_parser_combinators
  http://labs.enonic.com/articles/scalas-regular-expression-parser
  http://jim-mcbeath.blogspot.kr/2008/09/scala-parser-combinators.html#precedencerevisited
  http://stackoverflow.com/questions/11533547/operator-precedence-with-scala-parser-combinators
  http://kufli.blogspot.kr/2015/01/scala-parser-combinators-sql-parser.html
  https://dzone.com/articles/create-a-programming-language-with-scala-parser-co
  http://jim-mcbeath.blogspot.kr/2008/09/scala-parser-combinators.html#basic
  http://berniepope.id.au/docs/scala_parser_combinators.pdf
  http://www.donroby.com/wp/scala/parsing-expressions-with-scala-parser-combinators-2/
  https://www.cs.helsinki.fi/u/wikla/OTS/Sisalto/examples/html/ch31.html#sec1

  http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.142.5002&rep=rep1&type=pdf

  http://myltsev.name/ScalaDays2014/#/
  http://lihaoyi.github.io/fastparse/
  */

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


