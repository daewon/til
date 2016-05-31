import java.util.{TimeZone, GregorianCalendar, Calendar}

object Gigasecond {
  val GigaSecond: Int = 1000000000
  def apply(cal: Calendar): Gigasecond = new Gigasecond(cal)
}

class Gigasecond(cal: Calendar) {
  import Gigasecond._

  def date: Calendar = {
    val newCal = cal.clone().asInstanceOf[Calendar]
    newCal.add(Calendar.SECOND, GigaSecond)
    newCal
  }
}
