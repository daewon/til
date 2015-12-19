package io.daewon.til

trait Monad[+M, Cont[_]] {
  def unit[A](a: A): Cont[A]

  def flatMap[A](f: M => Cont[A]): Cont[A]

  def map[A](f: M => A): Cont[A]
}
