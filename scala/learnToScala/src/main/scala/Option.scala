package io.daewon.util

object Option {
  def apply[A](e: A): Option[A] = if (e != null) Some(e) else None
}

trait Option[+E] extends Monad[E, Option]{
  def isDefined: Boolean
  def flatMap[A](f: E => Option[A]): Option[A]
  def map[A](f: E => A): Option[A]
  def foreach[U](f: E => U): Unit
  def get(): E
  def getOrElse[A >: E](fallback: => A): A = if (isDefined) get() else fallback
  def find(pred: E => Boolean): Option[E]
}

final case class Some[+E](arg: E) extends Option[E] {
  override def isDefined: Boolean = true
  override def flatMap[A](f: E => Option[A]): Option[A] = f(arg)
  override def get(): E = arg
  override def foreach[U](f: E => U): Unit = f(arg)
  override def map[A](f: (E) => A): Option[A] = Some(f(arg))
  override def find(pred: (E) => Boolean): Option[E] = if (pred(arg)) this else None

  override def unit[A](a: A): Option[A] = Option(a)
}

final case object None extends Option[Nothing] {
  override def isDefined: Boolean = false
  override def flatMap[A](f: (Nothing) => Option[A]): Option[Nothing] = None
  override def get(): Nothing = throw new UnsupportedOperationException("get of None")
  override def foreach[U](f: (Nothing) => U): Unit = None
  override def map[A](f: (Nothing) => A): Option[A] = None
  override def find(pred: (Nothing) => Boolean): Option[Nothing] = None

  override def unit[A](a: A): Option[A] = None
}
