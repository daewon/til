object Binary {
  def apply(num: String) = new Binary(num)
}

class Binary(num: String) {
  def toDecimal: Int = num.foldLeft(Some(0): Option[Int]) {
    case (Some(n), '0') => Some(n << 1)
    case (Some(n), '1') => Some((n << 1) + 1)
    case _ => None
  } getOrElse 0
}
