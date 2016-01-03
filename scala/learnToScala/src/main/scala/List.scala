package io.daewon.util

import scala.annotation.{switch, tailrec}


// http://mauricio.github.io/2013/11/25/learning-scala-by-building-scala-lists.html

/**
  * Simple implementation LikedList
  * @tparam E
  */

object List {
  def apply[E](items: E*): List[E] =
    if (items.nonEmpty) Node(items.head, apply(items.tail: _*))
    else Empty
}

sealed trait List[+E] extends Monad[E, List] {

  def head: E

  def tail: List[E]

  def foreach(f: E => Unit) = {
    @tailrec def loop(acc: List[E]): Unit = acc match {
      case Empty =>
      case Node(head, tail) =>
        f(head)
        loop(tail)
    }

    loop(this)
  }

  def prepends[A >: E](other: List[A]): List[A] = other match {
    case Empty => this
    case Node(hd, tail) => hd :: prepends(tail)
  }

  def :::[A >: E](other: List[A]): List[A] = prepends(other)

  def ::[A >: E](e: A): List[A] = Node(e, this)

  val size: Int

  def filter(f: E => Boolean): List[E] = this match {
    case Empty => Empty
    case Node(head, tail) if f(head) => head :: tail.filter(f)
    case Node(head, tail) => tail.filter(f)
  }

  def reverse: List[E] = fold(Empty: List[E]) { case (acc, curr) =>
    Node(curr, acc)
  }

  def map[R](f: E => R): List[R]

  //  override def flatMap[A](f: (E) => Monad[A]): Monad[A] = ???
  //  def flatMap[B](f: E => List[B]): List[B] = fold(Empty: List[B]) {
  //    case (acc, current) => acc ::: f(current)
  //  }

  @tailrec final def fold[A](acc: A)(f: (A, E) => A): A = (this: @switch) match {
    case Node(head, tail) => tail.fold(f(acc, head))(f)
    case Empty => acc
  }

  def foldRight[A](acc: A)(f: (E, A) => A): A = this.reverse.fold(acc) { (acc, curr) =>
    f(curr, acc)
  }

  def find(pred: E => Boolean): Option[E] = this match {
    case Empty => None
    case Node(head, tail) if pred(head) => Some(head)
    case Node(_, tail) => tail.find(pred)
  }

  override def flatMap[A](f: (E) => List[A]): List[A] = fold(Empty: List[A]) {
    case (acc, current) => acc ::: f(current)
  }
}

case class Node[+E](val head: E, val tail: List[E]) extends List[E] {
  override val size = 1 + tail.size

  override def map[R](f: (E) => R): List[R] = Node(f(head), tail.map(f))

  override def unit[A](a: A): List[A] = List(a)
}

case object Empty extends List[Nothing] {
  override val size = 0

  override def map[R](f: Nothing => R): List[R] = this

  override def tail: Node[Nothing] = throw new UnsupportedOperationException("tail of empty list")

  override def head: Nothing = throw new UnsupportedOperationException("head of empty list")

  override def unit[A](a: A): List[A] = this
}
