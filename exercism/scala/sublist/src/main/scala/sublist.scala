import scala.annotation.tailrec

object Sublist {
  trait SublistType

  object Equal extends SublistType
  object Unequal extends SublistType
  object Sublist extends SublistType
  object Superlist extends SublistType
}

class Sublist {
  def sublist[A](as: List[A], bs: List[A]): Sublist.SublistType =
    if (as == bs) Sublist.Equal
    else if (isSublist(as, bs)) Sublist.Sublist
    else if (isSublist(bs, as)) Sublist.Superlist
    else Sublist.Unequal

  @tailrec
  final def isSublist[A](as: List[A], bs: List[A]): Boolean = (as, bs) match {
    case (Nil, _) => true
    case (_, Nil) => false
    case (_, bh :: bt) => startsWith(as, bh :: bt) || isSublist(as, bt)
  }

  @tailrec
  private def startsWith[A](as: List[A], bs: List[A]): Boolean = (as, bs) match {
    case (Nil, _) => true
    case (_, Nil) => false
    case (ah :: at, bh :: bt) if ah == bh => startsWith(at, bt)
    case (ah :: at, bh :: bt) => false
  }
}
