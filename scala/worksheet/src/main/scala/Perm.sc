def perm[T](ls: List[T]): List[List[T]] = ls match {
  case x :: Nil => List(ls)
  case _ => ls.flatMap { n =>
    perm(ls.filter(_ != n)).map { xs =>
      n :: xs
    }
  }
}

perm(List(1))
perm(List(1, 2))
perm(List(1, 2, 3))

