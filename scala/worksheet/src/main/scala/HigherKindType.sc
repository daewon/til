import scala.language.higherKinds

trait Functor[M[_]] {
  def map[A, B](cont: M[A], f: A => B): M[B]
}

trait Monad[M[_]] {
  def unit[A](v: A): M[A]

  def flatMap[A, B](cont: M[A], f: A => M[B]): M[B]
}

trait FoldM[M[_], T, U] {
  type S
  def start: M[S]
  def fold: (S, T) => S
  def end(s: S): M[U]
}

trait A {
  def a: String
  val b: String
}

class B extends A {
  override val a = ""
  override def b = ""
}


