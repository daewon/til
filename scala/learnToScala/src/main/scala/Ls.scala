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

  def foreach(f: E => Unit) = {
    @tailrec def loop(acc: Ls[E]): Unit = acc match {
      case Empty =>
      case Node(head, tail) =>
        f(head)
        loop(tail)
    }

    loop(this)
  }

  def prepends[A >: E](other: Ls[A]): Ls[A] = other match {
    case Empty => this
    case Node(hd, tail) => hd :: prepends(tail)
  }

  def :::[A >: E](other: Ls[A]): Ls[A] = prepends(other)

  def ::[A >: E](e: A): Ls[A] = Node(e, this)

  val size: Int

  def filter(f: E => Boolean): Ls[E] = this match {
    case Empty => Empty
    case Node(head, tail) if f(head) => head :: tail.filter(f)
    case Node(head, tail) => tail.filter(f)
  }

  def reverse: Ls[E] = fold(Empty: Ls[E]) { case (acc, curr) =>
    Node(curr, acc)
  }

  def map[R](f: E => R): Ls[R]

  def flatMap[B](f: E => Ls[B]): Ls[B] = fold(Empty: Ls[B]) {
    case (acc, current) => acc ::: f(current)
  }

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

  override def map[R](f: Nothing => R): Ls[R] = this
}
