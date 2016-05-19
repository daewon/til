import java.util.GregorianCalendar
import java.util.Calendar

object Meetup {
  trait Week
  val Sun = Calendar.SUNDAY
  val Mon = Calendar.MONDAY
  val Tue = Calendar.TUESDAY
  val Wed = Calendar.WEDNESDAY
  val Thu = Calendar.THURSDAY
  val Fri = Calendar.FRIDAY
  val Sat = Calendar.SATURDAY

  val Days = Stream.continually(Seq(Sun, Mon, Tue, Wed, Thu, Fri, Sat)).flatten

  def newCalendar(year: Int, month: Int, day: Int): Calendar = {
    val c = Calendar.getInstance()
    c.set(year, month-1, day)
    c
  }

  def dayOfWeek(c: Calendar): Int = c.get(Calendar.DAY_OF_WEEK);

  def days(c: Calendar): Seq[Int] = 1 to c.getActualMaximum(Calendar.DAY_OF_MONTH)

  def daysPair(c: Calendar) = days(c) zip Days.drop(dayOfWeek(c)-1)

  val validTeenth = 13 to 19 toSet // Magic number. I don't know why it's valid. I just passed test cases

  def validDays(cal: Calendar, w: Int) = daysPair(cal).filter { case (_, day) => day == w }.map(_._1)

  def apply(m: Int, y: Int) = new Meetup(m, y)
}

class Meetup(m: Int, y: Int) {
  import Meetup._

  val c = newCalendar(y, m, 1)

  def makeCalendar(day: => Int) = new GregorianCalendar(y, m-1, day)

  def teenth(w: Integer) = makeCalendar(validDays(c, w).filter { d => validTeenth(d) }.head)

  def first(w: Int) = makeCalendar(validDays(c, w).head)

  def second(w: Int) = makeCalendar(validDays(c, w)(1))

  def third(w: Int) = makeCalendar(validDays(c, w)(2))

  def fourth(w: Int) = makeCalendar(validDays(c, w)(3))

  def last(w: Int) = makeCalendar(validDays(c, w)last)
}
