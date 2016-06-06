object Binary {
  def apply(num: String) = new Binary(num)
}

class Binary(_num: String) {
  lazy val digits: Seq[String] = _num.split("")

  lazy val isValid: Boolean = digits.forall(c => c == "0" || c == "1")

  lazy val validDigits: Seq[Int] = if (isValid) digits.map(_.toInt) else Nil

  lazy val toDecimal: Int = validDigits
    .reverse
    .zipWithIndex
    .foldLeft(0) { case (acc, (n, base)) => acc + (n * Math.pow(2, base).toInt) }
}
