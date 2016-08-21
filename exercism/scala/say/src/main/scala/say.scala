object Say {
  val powers: Seq[(Long, String)] = Seq(
    (1000000000L, "billion"),
    (1000000L, "million"),
    (1000L, "thousand"),
    (100L, "hundred")
  )

  val numbers: Map[Long, String] = Seq(
    "", "one", "two", "three", "four", "five", "six", "seven",
    "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
    "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
  ).zipWithIndex.map { case (k, i) => i.toLong -> k } toMap

  val tens: Map[Long, String] = Seq(
    20L -> "twenty", 30L -> "thirty", 40L -> "forty", 50L -> "fifty",
    60L -> "sixty", 70L -> "seventy", 80L -> "eighty", 90L -> "ninety"
  ).toMap

  def inEnglish2(n: Long): String =
    if (n < 20) numbers(n)
    else if (n % 10 == 0) tens(n)
    else {
      val m = n / 10 * 10
      val r = n % 10
      s"${inEnglish2(m)}-${inEnglish2(r)}"
    }

  def inEnglish(n: Long): Option[String] = n match {
    case _ if n < 0 || n >= 1000000000000L => None
    case 0 => Option("zero")
    case _ =>
      val (rem, vals) = powers.foldLeft((n, Vector.empty[String])) { case ((r, ls), (k, str)) =>
        val m = r / k

        if (m > 0) (r % k, ls :+ s"${inEnglish(m).get} $str")
        else (r % k, ls)
      }

      val prepare = vals :+ inEnglish2(rem)
      Option(prepare.mkString(" ").trim)
  }
}
