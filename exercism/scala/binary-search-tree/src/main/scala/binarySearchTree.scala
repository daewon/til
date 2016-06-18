object Bst {
  def fromList[T: Ordering](from: Seq[T]): Bst[T] =
    from.tail.foldLeft(Bst(from.head)) { case (acc, e) =>
      acc.insert(e)
    }

  def toList[T](e: Bst[T]): Seq[T] = (e.left, e.right) match {
    case (None, None) => Seq(e.value)
    case (Some(l), Some(r)) => toList(l) ++ Seq(e.value) ++ toList(r)
    case (Some(l), _) => toList(l) ++ Seq(e.value)
    case (_, Some(r)) => Seq(e.value) ++ toList(r)
  }

  implicit class RichBst[T: Ordering](bst: Bst[T]) {
    def insert(e: T): Bst[T] = {
      val f = implicitly[Ordering[T]]
      val toRight = f.compare(bst.value, e) < 0

      (bst.left, bst.right, toRight) match {
        case (None, _, false) => bst.copy(left = Option(Bst(e)))
        case (Some(l), _, false) => bst.copy(left = Option(l.insert(e)))
        case (_, None, true) => bst.copy(right = Option(Bst(e)))
        case (_, Some(r), true) => bst.copy(right = Option(r.insert(e)))
      }
    }
  }
}

case class Bst[T](value: T, left: Option[Bst[T]] = None, right: Option[Bst[T]] = None)
