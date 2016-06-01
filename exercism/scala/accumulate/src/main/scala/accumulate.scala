class Accumulate {
  def accumulate[A, B](f: A => B, ls: Seq[A]): Seq[B] = ls match {
    case Nil => Nil
    case head +: tail => f(head) +: accumulate(f, tail)
  }
}
