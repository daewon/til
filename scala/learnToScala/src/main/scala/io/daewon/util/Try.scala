package io.daewon.util

import scala.util.control.NonFatal

object Try {
  {scala.util.Try}
  def apply[T](f: => T): Try[T] =
    try {
      Success(f)
    } catch {
      case NonFatal(ex) => Failure(ex)
    }
}

sealed trait Try[+A] extends Monad[A, Try] with Functor[A, Try] {
  override def unit[B](a: B): Try[B] = Try(a)

  def foreach[U](f: A => U): Unit

  def isSuccess: Boolean

  def isFailure: Boolean

  def getOrElse[B >: A](f: => B): B

  def get: A
}

case class Success[+A](a: A) extends Try[A] {
  override def foreach[U](f: A => U): Unit = map(f)

  override def isFailure: Boolean = false

  override def isSuccess: Boolean = true

  override def flatMap[B](f: A => Try[B]): Try[B] = f(a)

  override def map[B](f: A => B): Try[B] = Try(f(a))

  override def getOrElse[B >: A](f: => B): B = a

  override def get: A = a
}

case class Failure[+E <: Nothing](ex: Throwable) extends Try[E] {
  override def foreach[U](f: E => U): Unit = {}

  override def getOrElse[B >: E](f: => B): B = f

  override def isFailure: Boolean = true

  override def isSuccess: Boolean = false

  override def flatMap[A](f: E => Try[A]): Try[A] = this

  override def map[A](f: E => A): Try[A] = this

  override def get: E = throw ex
}
