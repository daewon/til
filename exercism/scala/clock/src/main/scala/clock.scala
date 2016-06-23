object Clock {
  def padRight(in: String, n: Int = 2): String = in.reverse.padTo(n, "0").mkString("").reverse

  def apply(h: Int, m: Int): Clock = {
    val (carry, min) =
      if (m < 0) m / 60 - 1 -> (m % 60 + 60)
      else m / 60 -> m % 60

    val hour = carry + h

    if (hour < 0) new Clock(hour % 24 + 24, min)
    else new Clock(hour % 24, min)
  }

  def apply(m: Int): Clock = Clock.apply(0, m)
}

class Clock(val h: Int, val m: Int) {
  import Clock._

  def +(o: Clock) = o match {
    case c: Clock => Clock(h + c.h, m + c.m)
  }

  def -(o: Clock) = o match {
    case c: Clock => Clock(h - c.h, m - c.m)
  }

  override def equals(o: Any) = o match {
    case c: Clock => c.h == h && c.m == m
    case _ => false
  }

  override def toString = {
    s"${padRight(h.toString)}:${padRight(m.toString)}"
  }

}
