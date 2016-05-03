object Year {
  def apply(year: Int) = new Year(year)
}

class Year(year: Int) {
  def evenly(n: Int) = year % n == 0
  def isLeap: Boolean = (evenly(400), evenly(100), evenly(4)) match {
    case (true, _, _) => true
    case (_, false, true) => true
    case _ => false
  }
}
