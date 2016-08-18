trait Ls[A] {
  def head: A

  def filter(p: A => Boolean): Ls[A]

  def tail: Ls[A]
}

class IStream[A](hd: A, tl: => IStream[A]) extends Ls[A] {
  override def head: A = hd

  override def filter(p: (A) => Boolean): IStream[A] =
    if (p(head)) new IStream(head, tail.filter(p))
    else tail.filter(p)

  override def tail: IStream[A] = tl
}

object IStream {
  def iterate[A](i: A)(f: (A => A)): IStream[A] = new IStream(i, iterate(f(i))(f))
}

val istream = IStream.iterate(1) { n => n + 1 }

istream.tail.filter(_.toString.contains("7")).head

object Ls {
  def concat[A](_head: A, _tail: => Ls[A]): Ls[A] = new Ls[A] {
    override def head: A = _head

    override def filter(p: (A) => Boolean): Ls[A] =
      if (p(head)) concat(head, tail.filter(p))
      else tail.filter(p)

    override def tail: Ls[A] = _tail
  }

  def iterate[A](i: A, f: (A => A)): Ls[A] = new Ls[A] {
    override def head: A = i

    override def tail: Ls[A] = iterate(f(i), f)

    override def filter(p: (A) => Boolean): Ls[A] =
      if (p(head)) concat(head, tail.filter(p))
      else tail.filter(p)
  }
}

val ints = Ls.iterate(1, a => a + 1)

ints.filter(_.toString.contains("5")).tail.tail.head

