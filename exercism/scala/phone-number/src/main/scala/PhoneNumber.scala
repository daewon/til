class PhoneNumber(input: String) {
  val valid = input
    .filter(_.toString.matches("[0-9]")).toList

  def number: String = (valid.length, valid) match {
    case (11, '1' :: tail) => tail.mkString("")
    case (10, _) => valid.mkString("")
    case _ => "0" * 10
  }

  def areaCode: String = number take 3
  def middle: String = (number drop 3) take 3
  def last: String = number drop 6

  override def toString = s"($areaCode) $middle-$last"
}
