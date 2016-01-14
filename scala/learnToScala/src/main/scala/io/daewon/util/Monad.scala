package io.daewon.util

trait Monad[+M, Cont[_]] {
  def unit[A](a: A): Cont[A]

  def flatMap[A](f: M => Cont[A]): Cont[A]
}
