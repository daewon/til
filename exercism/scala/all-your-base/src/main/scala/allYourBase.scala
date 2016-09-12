object AllYourBase {
  def isValid(srcBase: Int, digits: Seq[Int], tgtBase: Int): Option[List[Int]] = {
    val isInvalid = srcBase <= 1 || tgtBase <= 1 || digits.isEmpty || digits.exists(n => n >= srcBase || n < 0)

    if (isInvalid) None
    else Option(digits.toList)
  }

  def rebase(srcBase: Int, digits: Seq[Int], tgtBase: Int): Option[List[Int]] =
    isValid(srcBase: Int, digits: Seq[Int], tgtBase: Int).map { digits =>
      val decimal = baseToDecimal(srcBase, digits)
      decimalToBase(tgtBase, decimal)
    }

  def baseToDecimal(base: Int, digits: Seq[Int]): Int = {
    def product(in: (Int, Int)): Int = in._1 * in._2

    val init = Math.pow(base, digits.length-1).toInt
    Stream.iterate(init)(_ / base).zip(digits).map(product).sum
  }

  def decimalToBase(base: Int, n: Int): List[Int] =
    Stream.iterate(n)(_ / base).takeWhile(_ > 0).map(_ % base).reverse.toList
}
