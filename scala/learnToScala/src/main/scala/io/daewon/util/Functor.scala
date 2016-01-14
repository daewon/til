package io.daewon.util

import scala.language.higherKinds

trait Functor[M[_]] {
  def map[A, B](cont: M[A], f: A => B): M[B]
}
