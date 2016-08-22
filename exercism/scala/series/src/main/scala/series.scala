object Series {
  def sliding[T](ls: Seq[T], n: Int): List[List[T]] = ls match {
    case Nil => Nil
    case _ =>
      val hd = ls.take(n).toList
      if (hd.length >= n) hd :: sliding(ls.tail, n) else Nil
  }

  def slices(n: Int, in: String): List[List[Int]] = in match {
    case "" => Nil
    case _ =>
      // in.split("").map(_.toInt).toList.sliding(n).toList
      val ls = in.split("").map(_.toInt).toList
      sliding(ls, n)
  }
}
