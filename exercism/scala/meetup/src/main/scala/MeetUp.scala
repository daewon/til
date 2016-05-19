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

  def calendar(year: Int, month: Int, day: Int): Calendar = {
    val c = Calendar.getInstance()
    c.set(year, month-1, day)
    c
  }

  def dayOfWeek(c: Calendar): Int = c.get(Calendar.DAY_OF_WEEK);

  def days(c: Calendar): Seq[Int] =
    1 to c.getActualMaximum(Calendar.DAY_OF_MONTH)

  def daysPair(c: Calendar) = days(c) zip Days.drop(dayOfWeek(c)-1)

  def apply(m: Int, y: Int) = new Meetup(m, y)
}

class Meetup(m: Int, y: Int) {
  import Meetup._

  val cal = calendar(y, m, 1)

  def targets(w: Int) = daysPair(cal).filter { case (_, day) => day == w }

  def teenth(w: Integer) = {
    new GregorianCalendar(y, m-1, 13)
  }

  def first(w: Int) = {
    val day = targets(w).head._1
    new GregorianCalendar(y, m-1, day)
  }

  def second(w: Int) = {
    val day = targets(w).drop(1).head._1
    new GregorianCalendar(y, m-1, day)
  }

  def last(w: Int) = {
    val day = targets(w).last._1
    new GregorianCalendar(y, m-1, day)
  }

}
