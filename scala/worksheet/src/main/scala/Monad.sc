trait Monad[+M] {
  def flatMap[A, B](f: M => Monad[A]): Monad[A]
  def map[A](f: M => A): Monad[A]
  def unit[A](a: A): Monad[A]
}


