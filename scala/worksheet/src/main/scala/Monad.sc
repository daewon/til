class Maybe[+T]

case class Just[T](value: T) extends Maybe[T]

case object None extends Maybe[Nothing]

case class Value[T](value: T)

trait Monad[C[_]] {
  def pure[A](a: A): C[A]

  def bind[A, B](a: C[A], f: A => C[B]): C[B]
}

object Monad {

  implicit object MaybeMonad extends Monad[Maybe] {
    override def bind[A, B](a: Maybe[A], f: A => Maybe[B]): Maybe[B] = a match {
      case Just(x) => f(x)
      case None => None
    }

    override def pure[A](a: A): Maybe[A] = Just(a)
  }


  implicit object IdentityMonad extends Monad[Value] {
    override def bind[A, B](a: Value[A], f: A => Value[B]): Value[B] = f(a.value)

    override def pure[A](a: A): Value[A] = Value(a)
  }

}

trait MA[M[_], A] {
  val value: M[A]

  def bind[B](f: A => M[B])(implicit m: Monad[M]) = m bind(value, f)
}

def pure[M[_], A](a: A)(implicit m: Monad[M]): M[A] = m pure a

def just[A](a: A): Maybe[A] = Just(a)

implicit def toMA[M[_], A](ma: M[A]) = new {
  val value: M[A] = ma
} with MA[M, A]

println(just(1) bind (pure[Maybe, Int](_)))
println(Value(1) bind (pure[Value, Int](_)))
