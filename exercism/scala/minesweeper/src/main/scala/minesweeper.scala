object Minesweeper {
  def annotate(in: Seq[String]): Seq[String] = in match {
    case Nil => Nil
    case _ =>
      val m = in.map(_.split(""))
      val rSize = m.length
      val colSize = m(0).length

      Array.tabulate(rSize, colSize) { (r, c) =>
        m(r)(c) match {
          case " " =>
            val count = arounds(r, c)
              .filter { case (x, y) => valid(x, y, rSize, colSize) && m(x)(y) == "*" }
              .length

            if (count == 0) " " else count.toString
          case _ => "*"
        }
      }.map(_.mkString(""))
  }

  def valid(r: Int, c: Int, rLimit: Int, cLimit: Int): Boolean =
    r > -1 && c > -1 && r < rLimit && c < cLimit

  def arounds(r: Int, c: Int): Seq[(Int, Int)] = Seq(
    (r-1, c-1), (r, c-1), (r+1, c-1),
    (r-1, c),             (r+1, c),
    (r-1, c+1), (r, c+1), (r+1, c+1)
  )
}
