import scala.annotation.tailrec

class Accumulate {
  def accumulate[A, B](f: A => B, ls: List[A]): List[B] = {
    @tailrec def inner(ls: List[A], acc: List[B] = Nil): List[B] = ls match {
      case Nil => acc
      case head :: tail => inner(tail, f(head) :: acc)
    }

    inner(ls).reverse
  }
}
