package io.daewon.til

trait Monad[+M] {
//  def unit[A](a: A): Monad[A]

  def flatMap[A](f: M => Monad[A]): Monad[A]

//  def map[A](f: M => A): Monad[A]
}
