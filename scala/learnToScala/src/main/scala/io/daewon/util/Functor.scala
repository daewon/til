package io.daewon.util

import scala.language.higherKinds

trait Functor[+A, M[_]] {
  def map[B](f: A => B): M[B]
}
