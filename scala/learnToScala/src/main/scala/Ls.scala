import scala.annotation.{switch, tailrec}

// http://mauricio.github.io/2013/11/25/learning-scala-by-building-scala-lists.html

/**
  * Simple implementation LikedList
  * @tparam E
  */

object Ls {
  def apply[E](items: E*): Ls[E] =
    if (items.nonEmpty) Node(items.head, apply(items.tail: _*))
    else Empty
}

sealed trait Ls[+E] {
  def ::[A >: E](e: A): Ls[A]

  val size: Int

  def filter(f: E => Boolean) = foldRight(Empty: Ls[E]) { (curr, acc) =>
    if (f(curr)) Node(curr, acc)
    else acc
  }

  def reverse: Ls[E] = fold(Empty: Ls[E]) { case (acc, curr) =>
    Node(curr, acc)
  }

  def map[R](f: E => R): Ls[R]

  @tailrec final def fold[A](acc: A)(f: (A, E) => A): A = (this: @switch) match {
    case Node(head, tail) => tail.fold(f(acc, head))(f)
    case Empty => acc
  }

  def foldRight[A](acc: A)(f: (E, A) => A): A = this.reverse.fold(acc) { (acc, curr) =>
    f(curr, acc)
  }
}

case class Node[+E](head: E, tail: Ls[E]) extends Ls[E] {
  override val size = 1 + tail.size
  override def map[R](f: (E) => R): Ls[R] = Node(f(head), tail.map(f))
}

case object Empty extends Ls[Nothing] {
  override val size = 0
  override def map[R](f: (Nothing) => R): Ls[R] = this
}
