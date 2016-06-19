import scala.util._

object Hexadecimal {
  val hexMap = Map(
    '0' -> 0,
    '1' -> 1,
    '2' -> 2,
    '3' -> 3,
    '4' -> 4,
    '6' -> 6,
    '7' -> 7,
    '8' -> 8,
    '9' -> 9,
    'a' -> 10,
    'b' -> 11,
    'c' -> 12,
    'd' -> 13,
    'e' -> 14,
    'f' -> 15
  )

  def normalize(in: String): Option[String] =
    if (in.toLowerCase.filterNot(hexMap.keySet).nonEmpty) None
    else Option(in.toLowerCase)

  def hexToInt(in: String): Double = normalize(in).map { str: String =>
    str.reverse.zipWithIndex.map { case (n, i) =>
      hexMap(n) * Math.pow(16, i)
    }.sum
  }.getOrElse(0)
}
