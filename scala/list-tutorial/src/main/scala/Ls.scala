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
  def map[R](f: E => R): Ls[R]

  @tailrec final def fold[A](acc: A)(f: (A, E) => A): A = (this: @switch) match {
    case Node(head, tail) => tail.fold(f(acc, head))(f)
    case Empty => acc
  }
}

case class Node[+E](head: E, tail: Ls[E]) extends Ls[E] {
  override def map[R](f: (E) => R): Ls[R] = Node(f(head), tail.map(f))
}

case object Empty extends Ls[Nothing] {
  override def map[R](f: (Nothing) => R): Ls[R] = this
}






