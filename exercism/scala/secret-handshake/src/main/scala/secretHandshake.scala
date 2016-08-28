import scala.util._

object SecretHandshake {
  val cmdMap = Map(
    1 -> "wink",
    2 -> "double blink",
    4 -> "close your eyes",
    8 -> "jump"
  )

  def handshake(n: String): List[String] =
    Try(handshake(Integer.parseInt(n, 2))).recover { case _ => Nil } get

  def handshake(n: Int): List[String] = n match {
    case 0 => Nil
    case n if n == 1 || n % 2 == 0 => List(cmdMap(n))
    case _ =>
      val m = n & 15 // split last for digit (0 | 0000)
      val ls = Stream.iterate((m + 1) >> 1)(_ >> 1).takeWhile(_ > 0).toList

      if (n > 15) ls.flatMap(handshake) else ls.flatMap(handshake).reverse
  }
}
