def permutation[A](ls: List[A]): List[List[A]] = ls match {
  case Nil => List(Nil)
  case list => list flatMap { hd =>
    permutation(ls.filterNot(_ == hd)).map(hd :: _)
  }
}

permutation(List("a", "b", "c")).map(_.mkString(""))