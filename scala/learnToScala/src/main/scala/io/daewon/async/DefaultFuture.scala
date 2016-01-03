package io.daewon.async


import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object CurrentThreadExecutionContext extends ExecutionContext {

  def execute(runnable: Runnable): Unit = runnable.run()

  def reportFailure(t: Throwable): Unit = t.printStackTrace()

}

class DefaultFuture[A] extends Future[A] {

  case class FutureCallback(func: Try[A] => Any, ec: ExecutionContext)

  @volatile private var result: Try[A] = null
  private var callbacks = Vector.empty[FutureCallback]

  override def isCompleted: Boolean = result != null

  override def value: Option[Try[A]] = this.isCompleted match {
    case true => Some(result)
    case false => None
  }

  def complete(_value: Try[A]): Unit = _value match {
    case null => throw new IllegalStateException("A future can't be complete with null")
    case _ => synchronized {
      if (!this.isCompleted) {
        result = _value
        fireCallbacks()
      }
    }
  }

  def flatMap[S](f: (A) => Future[S])(implicit executor: ExecutionContext): Future[S] = {
    val p = Promise[S]()
    onComplete {
      case Success(v) => try {
        f(v).onComplete(p.complete)
      } catch {
        case NonFatal(e) => p.failure(e)
      }
      case Failure(e) => p.failure(e)
    }
    p.future
  }

  def map[S](f: (A) => S)(implicit executor: ExecutionContext): Future[S] = {
    val p = Promise[S]()
    onComplete { v => p complete (v map f) }
    p.future
  }

  override def onComplete[U](f: (Try[A]) => U)(implicit ec: ExecutionContext): Unit = {
    val callback = FutureCallback(f, ec)
    this.synchronized {
      this.isCompleted match {
        case true => fireCallback(callback)
        case false => callbacks = callback +: callbacks
      }
    }
  }

  private def fireCallback(cb: FutureCallback): Unit = {
    cb.ec.execute(new Runnable {
      override def run(): Unit = cb.func(result)
    })
  }

  private def fireCallbacks(): Unit = {
    callbacks.foreach(fireCallback)
    callbacks = Vector.empty[FutureCallback]
  }

}
