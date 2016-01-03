package io.daewon.async


import io.daewon.util._

import scala.concurrent.ExecutionContext

object CurrentThreadExecutionContext extends ExecutionContext {
  def execute(runnable: Runnable): Unit = runnable.run()

  def reportFailure(t: Throwable): Unit = t.printStackTrace()
}

class DefaultFuture[A] extends Future[A] {

  case class FutureCallback(func: Try[A] => Any, ec: ExecutionContext)

  object status {
    @volatile var value: Try[A] = null
    var callbacks = Vector.empty[FutureCallback]
  }

  override def isCompleted: Boolean = status.value != null

  override def value: Option[Try[A]] = this.isCompleted match {
    case true => Option(status.value)
    case false => None
  }

  def complete(_value: Try[A]): Unit = _value match {
    case null => throw new IllegalStateException("A Future can't be completed with null")
    case _ => synchronized {
      if (this.isCompleted) throw new IllegalStateException("Promise already completed.")
      status.value = _value
      fireCallbacks()
    }
  }

  override def flatMap[B](f: A => Future[B])(implicit ec: ExecutionContext): Future[B] = {
    val p = Promise[B]()
    this onComplete {
      case Success(v) => f(v) onComplete p.complete
      case Failure(ex) => p.failure(ex)
    }
    p.future
  }

  override def map[B](f: A => B)(implicit ec: ExecutionContext): Future[B] = {
    val p = Promise[B]()
    this onComplete {
      v => p complete (v map f)
    }
    p.future
  }

  override def onComplete[U](f: Try[A] => U)(implicit ec: ExecutionContext): Unit = {
    val callback = FutureCallback(f, ec)
    this.status.synchronized {
      this.isCompleted match {
        case true => fireCallback(callback)
        case false => status.callbacks = callback +: status.callbacks
      }
    }
  }

  private def fireCallback(futureCallback: FutureCallback): Unit = {
    futureCallback.ec.execute(new Runnable {
      override def run(): Unit = futureCallback.func(status.value)
    })
  }

  private def fireCallbacks(): Unit = {
    status.callbacks.foreach(fireCallback)
    status.callbacks = Vector.empty[FutureCallback]
  }
}
