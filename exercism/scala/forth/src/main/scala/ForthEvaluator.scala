import ForthError.ForthError

object ForthError extends Enumeration {
  type ForthError = Value
  val DivisionByZero, StackUnderflow, InvalidWord, UnknownWord = Value
}

sealed trait Expr
case class UDF(name: String, cmd: Seq[Expr]) extends Expr
case class Number(value: Int) extends Expr
case class Cmd(cmd: String) extends Expr

trait ForthEvaluatorState {
  // TODO: Implement. return the current stack as Text with the element
  // on top of the stack being the rightmost element in the output."
  override def toString: String
}

case class EvaluatorState(stack: List[Expr]) extends ForthEvaluatorState {
  override def toString = stack.reverse.collect { case Number(n) => n } mkString(" ")
}

abstract class Definition {
  def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState]
}

trait ForthEvaluator  {
  def eval(text: String): Either[ForthError, ForthEvaluatorState]
}

object Forth {
  def parse(in: String): List[Expr] = {
    val tokens = """(-?\d+)|([\p{L}\p{Sc}\-]+)|(\s[-+*/])""".r
    val UdfRe = """(?s)\s*(:.+?;)(.*)""".r
    val NumberRe = """(\d+)""".r
    val CmdRe = """([\p{L}\p{Sc}\-]+)""".r
    val OpRe = """([-+*/]{1})""".r
    val SymRe = """[\p{L}\p{Sc}\-]+""".r

    def parseToken(token: String): List[String] = token match {
      case UdfRe(udf, after) => udf :: parseToken(after)
      case cmd => tokens.findAllMatchIn(cmd).flatMap(_.subgroups).filterNot(_ == null).toList
    }

    val parsed = parseToken(in.toUpperCase)
    parsed.map(_.trim).map {
      case UdfRe(udf, _) =>
        val Array(name, cmd@_*) = udf.trim.tail.init.split(" ").filter(_.trim.nonEmpty)
        if (SymRe.unapplySeq(name).isEmpty) throw new IllegalArgumentException("UDF name is Number")
        else UDF(name, parse(cmd.mkString(" ")))
      case NumberRe(n) => Number(n.toInt)
      case CmdRe(cmd) => Cmd(cmd)
      case OpRe(op) => Cmd(op)
      case ex@_ => throw new RuntimeException(s"Unsupported expr: '${ex}'")
    }.toList
  }

  type ForthFn = Seq[Expr] => Seq[Number]

  val builtIn: Map[Cmd, ForthFn] = Map(
    Cmd("+") -> ((args: Seq[Number]) => {
      val a :: b :: rest = args
      Number(a.value + b.value) :: rest
    }),
    Cmd("-") -> ((args: Seq[Number]) => {
      val a :: b :: rest = args
      Number(b.value - a.value) :: rest
    }),
    Cmd("*") -> ((args: Seq[Number]) => {
      val a :: b :: rest = args
      Number(a.value * b.value) :: rest
    }),
    Cmd("/") -> ((args: Seq[Number]) => {
      val a :: b :: rest = args
      Number(b.value / a.value) :: rest
    }),
    Cmd("DUP") -> ((args: Seq[Number]) => {
      args.head +: args
    }),
    Cmd("DROP") -> ((args: Seq[Number]) => {
      if (args.isEmpty) throw new NoSuchElementException
      args.tail
    }),
    Cmd("SWAP") -> ((args: Seq[Number]) => {
      if (args.lengthCompare(2) < 0) throw new NoSuchElementException
      val a :: b :: rest = args
      b :: a :: rest
    }),
    Cmd("OVER") -> ((args: Seq[Number]) => {
      if (args.lengthCompare(2) < 0) throw new NoSuchElementException
      val a :: b :: rest = args
      b :: a :: b :: rest
    })
  )
}

class Forth extends ForthEvaluator {

  def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
    scala.util.Try {
      val cmds = Forth.parse(text)
      val ret = evalInner(cmds, Nil, Forth.builtIn)
      Right(EvaluatorState(ret))
    } recover {
      case _: ArithmeticException => Left(ForthError.DivisionByZero)
      case _: NoSuchElementException => Left(ForthError.StackUnderflow)
      case _: IllegalArgumentException => Left(ForthError.InvalidWord)
    } get
  }

  def evalInner(cmds: Seq[Expr], stack: List[Number], env: Map[Cmd, Seq[Number] => Seq[Number]]): List[Number] = cmds match {
    case Number(n) :: tl => evalInner(tl, Number(n) :: stack, env)
    case (u@UDF(name, udf)) :: tl =>
      val f = (st: Seq[Number]) => evalInner(udf, st.toList, env)

      evalInner(tl, stack, env + (Cmd(name) -> f))
    case Cmd(cmd) :: tl =>
      val f = env.getOrElse(Cmd(cmd), throw new IllegalArgumentException)
      evalInner(tl, f(stack).toList, env)

    case Nil => stack
  }
}
