object Luhn {
  implicit class RichLong(val n: Long) extends AnyVal {
    def digits: Seq[Int] = n match {
      case 0 => Seq(0)
      case _ =>
        var nums: List[Int] = Nil
        var remain: Long = n

        while(remain > 0) {
          nums = (remain % 10).toInt :: nums
          remain = remain / 10
        }

        nums
    }

    def isEven: Boolean = n % 2 == 0
  }
}

case class Luhn(n: Long) {
  import Luhn._

  def checkDigit: Long = n.digits.last

  def addends: List[Int] = {
    n.digits.reverse.zipWithIndex.map { case (n, idx) =>
      if (idx.isEven) n
      else {
        val m = n * 2
        if (m < 10) m
        else m - 9
      }
    }.reverse.toList
  }

  def checksum: Long = addends.sum.digits.last

  def isValid: Boolean = checksum == 0

  def create: Long = {
    (0 to 9)
      .map { m: Int => s"${n}${m}".toLong }
      .filter { a => Luhn(a).isValid }
      .head
  }
}
