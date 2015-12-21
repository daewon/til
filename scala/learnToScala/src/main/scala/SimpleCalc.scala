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

  object ExprParser extends StandardTokenParsers {
    lexical.delimiters ++= Set("+", "-")

    def value = numericLit ^^ { s => EConst(s.toDouble) }

    def sum = value * (
      "+" ^^^ { (a: Expr, b: Expr) => EAdd(a, b) } |
        "-" ^^^ { (a: Expr, b: Expr) => ESub(a, b) }
      )

    def expr = sum | value //top level expression

    def parse(s: String) = {
      val tokens = new lexical.Scanner(s)
      phrase(expr)(tokens)
    }

    def apply(s: String): Expr = {
      parse(s) match {
        case Success(tree, _) => tree
        case e: NoSuccess =>
          throw new IllegalArgumentException("Bad syntax: " + s)
      }
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


