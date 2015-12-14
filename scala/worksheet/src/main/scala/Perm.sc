def perm[T](ls: List[T]): List[List[T]] = ls match {
  case x :: Nil => List(ls)
  case _ => ls.flatMap { n =>
    perm(ls.filter(_ != n)).map { xs =>
      n :: xs
    }
  }
}

def perm2[T](ls: List[T]): List[List[T]] = ls match {
  case x :: Nil => List(ls)
  case _ => for {
    n <- ls
    xs <- perm2(ls.filter(_ != n))
  } yield n :: xs
}

perm(List(1))
perm(List(1, 2))
perm(List(1, 2, 3))

perm2(List(1))
perm2(List(1, 2))
perm2(List(1, 2, 3))
