case class Matrix(in: String) {
  val m: Seq[Seq[Int]] =
    in.split("\n").map(_.split(" ").map(_.toInt).toSeq)

  def rows(n: Int): Seq[Int] = m(n)

  def cols(n: Int): Seq[Int] = m.transpose.toSeq(n)
}
