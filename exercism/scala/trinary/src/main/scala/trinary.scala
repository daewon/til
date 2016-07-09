object Trinary {

  def trinaryToInt(in: String): Int =
    if (in.exists(_.toString.toInt > 2)) throw new IllegalArgumentException
    else if (in.toInt == 0) 0 // if `in` contains non-digit char, `toInt` throw IllegalArgumentException
    else {
      val pows = Stream.iterate(Math.pow(3, in.size-1).toInt)(_ / 3)
      in map(_ - 48) zip pows map { case (n, p) => n * p } sum
    }

  def intToTrinary(n: Int): String = {
    val nums = Stream.iterate(n)(_ / 3).takeWhile(_ > 0) map(_ % 3) reverse

    if (nums.isEmpty) "0"
    else nums.mkString("")
  }
}
