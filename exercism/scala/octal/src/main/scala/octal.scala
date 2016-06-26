object Octal {
  def intToOctal(n: Int): String = {
    val stream = Stream.iterate(n)(n => n / 8)
    val nums = stream.takeWhile(_ > 0).map(_ % 8).reverse

    if (nums.forall(_ == 0)) "0"
    else nums.mkString("")
  }

  def validNum: Set[Int] = (0 to 7).toSet

  def octalToInt(in: String): Int = {
    if (in.length == 0) throw new IllegalArgumentException
    if (!in.forall(n => validNum(n.toString.toInt))) throw new IllegalArgumentException

    in.reverse.zipWithIndex.map { case (n, idx) =>
      (n.toString.toInt * Math.pow(8, idx)).toInt
    }.sum
  }
}
