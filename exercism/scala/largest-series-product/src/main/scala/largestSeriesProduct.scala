object Series {
  def largestProduct(n: Int, in: String): Option[Long] =
    if (n > in.length) None
    else if (n == 0) Some(1)
    else Option(in.split("").map(_.toInt).sliding(n, 1).map(_.product).max)
}
