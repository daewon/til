case class Matrix(in: Seq[Seq[Int]]) {

  val minRows: Seq[Int] = in.map(_.max).toArray

  val minCols: Seq[Int] = in.transpose.map(_.min).toArray

  val len: Int = in.length

  // (9, 8, 7)
  // (5, 3, 2)
  // (6, 6, 7)
  // == (1, 0) max in row, min in col
  def saddlePoints: Set[(Int, Int)] = {
    var ret = for {
      row <- 0 until len
      col <- 0 until len
      if in(row)(col) == minRows(row) && in(row)(col) == minCols(col)
    } yield (row, col)

    ret.toSet
  }
}
