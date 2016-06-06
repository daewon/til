object RomanNumeral {
  val RomanNumerals = Vector(
    1000 -> "M", 900 -> "CM", 500 -> "D", 400 -> "CD",
    100 -> "C", 90 -> "XC", 50 -> "L", 40 -> "XL",
    10 -> "X", 9 -> "IX", 5 -> "V", 4 -> "IV",
    1 -> "I"
  )

  def apply(n: Int) = new RomanNumeral(n)
}

class RomanNumeral(n: Int) {
  import RomanNumeral._

  lazy val value = {
    var amount = n
    var idx = 0
    var acc: Vector[String] = Vector.empty

    while(amount > 0) {
      val (num, roman) = RomanNumerals(idx)
      if (amount >= num) {
        amount -= num
        acc = acc :+ roman
      } else {
        idx = idx + 1
      }
    }

    acc.mkString("")
  }
}
