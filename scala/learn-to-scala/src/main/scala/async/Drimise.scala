package io.dun.async

import scala.util._

/**
  * http://mauricio.github.io/2014/05/01/scala-promises-futures-memcached-and-netty-having-fun.html
  * @tparam T
  */
case class Drimise[T]() {
  @volatile private var result: Try[T] = null

  def isCompleted: Boolean = result != null

  def value: Try[T] =
    if (isCompleted) result
    else throw new IllegalStateException("This promise is not completed yet")


  def complete(_result: Try[T]): Drimise[T] =
    if (!tryComplete(_result)) throw new IllegalStateException("Promise already completed")
    else this

  def tryComplete(_result: Try[T]): Boolean =
    if (_result == null) throw new IllegalStateException("Result can't be null")
    else synchronized {
      if (isCompleted) false
      else {
        result = _result
        true
      }
    }

  def success(value: T): this.type = complete(Success(value))
  def trySuccess(value: T): Boolean = tryComplete(Success(value))
  def failure(ex: Throwable): this.type = complete(Failure(ex))
  def tryFailure(ex: Throwable): Boolean = tryComplete(Failure(ex))
}

object Main {
  val p = Drimise[Int]()
  p.complete(Try(10))
}

