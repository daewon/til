package io.daewon.async

import scala.concurrent.ExecutionContext
import io.daewon.util._

trait Future[+A] {
  def isCompleted: Boolean

  def value: Option[Try[A]]

  def flatMap[S](f: (A) => Future[S])(implicit ec: ExecutionContext): Future[S]

  def map[S](f: (A) => S)(implicit ec: ExecutionContext): Future[S]

  def foreach[U](f: (A) => Unit)(implicit ec: ExecutionContext): Unit = map(f)

  def onComplete[U](f: Try[A] => U)(implicit ec: ExecutionContext): Unit

  def onFailure[U](pf: PartialFunction[Throwable, U])(implicit ec: ExecutionContext) = onComplete {
    case Failure(e) => pf.apply(e)
    case _ =>
  }

  def onSuccess[U](pf: PartialFunction[A, U])(implicit ec: ExecutionContext): Unit = onComplete {
    case Success(v) => pf.apply(v)
    case _ =>
  }
}
