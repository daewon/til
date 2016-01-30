type M[A] = List[A]

def pure[A](a: A): M[A] = List(a)

def bind[A, B](m: M[A])(k: A => M[B]): M[B] = m match {
  case Nil => Nil
  case hd :: tl => k(hd) ++ bind(tl)(k)
}

val lsm = pure(1)

bind(lsm) { n =>
  pure(n * 2)
}


