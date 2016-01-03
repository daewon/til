package io.daewon.async

import scala.util._

/**
  * http://mauricio.github.io/2014/05/01/scala-promises-futures-memcached-and-netty-having-fun.html
  * @tparam A
  */
case class Promise[A]() {
  @volatile private var result: Try[A] = null
  private lazy val internalFuture = new DefaultFuture[A]()

  def isCompleted: Boolean = result != null

  def value: Try[A] =
    if (isCompleted) result
    else throw new IllegalStateException("This promise is not completed yet")

  def complete(_result: Try[A]): this.type =
    if (!tryComplete(_result)) throw new IllegalStateException("Promise already completed")
    else this

  def tryComplete(result: Try[A]): Boolean = {
    if (result == null) throw new IllegalStateException("result can't be null")

    synchronized {
      if (isCompleted) {
        false
      } else {
        this.result = result
        this.internalFuture.complete(result)
        true
      }
    }
  }

  def success(value: A): this.type = complete(Success(value))

  def trySuccess(value: A): Boolean = tryComplete(Success(value))

  def failure(ex: Throwable): this.type = complete(Failure(ex))

  def tryFailure(ex: Throwable): Boolean = tryComplete(Failure(ex))

  def future: Future[A] = internalFuture
}
