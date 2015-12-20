package io.daewon.til

import scala.util.parsing.combinator._

object SimpleCalc {

  trait Exp {
    def eval: Int
  }

  case class Num(n: Int) extends Exp {
    override def eval: Int = n
  }

  case class Add(a: Exp, b: Exp) extends Exp {
    override def eval: Int = a.eval + b.eval
  }

  class Arith extends JavaTokenParsers {
    def term: Parser[Exp] = factor ~ rep("+" ~ factor) ^^ {
      case fact ~ ls => Num(fact.eval + ls.foldLeft(0) { case (acc, current) =>
        val value = current match {
          case s ~ fn => fn.eval
        }
        acc + value
      })
    }

    def factor: Parser[Exp] =
      floatingPointNumber ^^ { case n => Num(n.toInt) } | "(" ~> term <~ ")" ^^ {
        case expr => expr
      }

    def eval(exp: String) = parseAll(term, exp) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }

  val parser = new Arith

  def parse(str: String) = parser.eval(str)
}


