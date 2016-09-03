import ForthError.ForthError

object ForthError extends Enumeration {
  type ForthError = Value
  val DivisionByZero, StackUnderflow, InvalidWord, UnknownWord = Value
}

sealed trait Expr
case class UDF(name: String, cmd: Seq[Expr]) extends Expr
case class Number(value: Int) extends Expr
case class Cmd(cmd: String) extends Expr

trait ForthEvaluator  {
  def eval(text: String): Either[ForthError, ForthEvaluatorState]
}

trait ForthFn extends (EvaluatorState => EvaluatorState)

trait ForthEvaluatorState {
  // TODO: Implement. return the current stack as Text with the element
  // on top of the stack being the rightmost element in the output."
  override def toString: String
}

case class EvaluatorState(stack: List[Number], env: Map[Cmd, ForthFn]) extends ForthEvaluatorState {

  def push(exp: Number) = copy(stack = exp :: stack)

  def pop(): (Number, EvaluatorState) = (stack.head, copy(stack = stack.tail))

  def addEnv(name: String)(f: ForthFn): EvaluatorState = this.copy(env = env + (Cmd(name) -> f))

  def apply(name: String): EvaluatorState = {
    val f = env.getOrElse(Cmd(name), throw new IllegalArgumentException)
    f(this)
  }

  override def toString = stack.reverse.collect { case Number(n) => n } mkString(" ")
}

abstract class Definition {
  def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState]
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

  val builtIn: Map[Cmd, ForthFn] = Map(
    Cmd("+") -> new ForthFn {
      def apply(ev: EvaluatorState): EvaluatorState = {
        val (a, ev2) = ev.pop
        val (b, ev3) = ev2.pop
        ev3.push(Number(a.value + b.value))
      }
    },
    Cmd("-") -> new ForthFn {
      def apply(ev: EvaluatorState): EvaluatorState = {
        val (a, ev2) = ev.pop
        val (b, ev3) = ev2.pop
        ev3.push(Number(b.value - a.value))
      }
    },
    Cmd("*") -> new ForthFn {
      def apply(ev: EvaluatorState): EvaluatorState = {
        val (a, ev2) = ev.pop
        val (b, ev3) = ev2.pop
        ev3.push(Number(a.value * b.value))
      }
    },
    Cmd("/") -> new ForthFn {
      def apply(ev: EvaluatorState): EvaluatorState = {
        val (a, ev2) = ev.pop
        val (b, ev3) = ev2.pop
        ev3.push(Number(b.value / a.value))
      }
    },
    Cmd("DUP") -> new ForthFn {
      def apply(ev: EvaluatorState): EvaluatorState = {
        val (a, ev2) = ev.pop
        ev.push(a)
      }
    },
    Cmd("DROP") -> new ForthFn {
      def apply(ev: EvaluatorState): EvaluatorState = {
        val (a, ev2) = ev.pop
        ev2
      }
    },
    Cmd("SWAP") -> new ForthFn {
      def apply(ev: EvaluatorState): EvaluatorState = {
        val (a, ev2) = ev.pop
        val (b, ev3) = ev2.pop
        ev3.push(a).push(b)
      }
    },
    Cmd("OVER") -> new ForthFn {
      def apply(ev: EvaluatorState): EvaluatorState  = {
        val (a, ev2) = ev.pop
        val (b, ev3) = ev2.pop
        ev3.push(b).push(a).push(b)
      }
    }
  )
}

class Forth extends ForthEvaluator {

  def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
    scala.util.Try {
      val cmds = Forth.parse(text)
      Right(evalInner(cmds, EvaluatorState(Nil, Forth.builtIn)))
    } recover {
      case _: ArithmeticException => Left(ForthError.DivisionByZero)
      case _: NoSuchElementException => Left(ForthError.StackUnderflow)
      case _: IllegalArgumentException => Left(ForthError.InvalidWord)
    } get
  }

  def evalInner(cmds: List[Expr], es: EvaluatorState): EvaluatorState = cmds match {
    case Number(n) :: tl => evalInner(tl, es.push(Number(n)))
    case (u@UDF(name, udf)) :: tl =>
      val f = new ForthFn {
        def apply(ev: EvaluatorState) = evalInner(udf.toList, ev)
      }
      evalInner(tl, es.addEnv(name)(f))
    case Cmd(name) :: tl => evalInner(tl, es.apply(name))
    case Nil => es
  }
}
