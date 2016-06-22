object Clock {
  def padRight(in: String, n: Int = 2): String = in.reverse.padTo(n, "0").mkString("").reverse

  def apply(_h: Int, _m: Int): Clock = {
    val (hp, m) = (_m / 60, _m % 60)
    println(hp -> m)

    if (_h < 0) {
      val h = hp + _h
      new Clock(h % 24 + 24 , m)
    } else {
      val h = hp + _h
      new Clock(h % 24 , m)
    }

    // val h = if (_h < 0) _h % 24 + 24 else _h % 24
    // val m = if (_m < 0) _m % 60 + 60 else _m % 60

    // if (h < 0) new Clock(h + (_m / 60 % 24), m)
    // else new Clock(h + (_m / 60 % 24), m)
  }

  def apply(m: Int): Clock = Clock.apply(0, m)
}

class Clock(val h: Int, val m: Int) {
  import Clock._

  override def toString = {
    s"${padRight(h.toString)}:${padRight(m.toString)}"
  }

  override def equals(o: Any) = o match {
    case c: Clock => c.h == h && c.m == m
    case _ => false
  }
}
