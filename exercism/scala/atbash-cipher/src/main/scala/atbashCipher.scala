object Atbash {
  private val org =     "abcdefghijklmnopqrstuvwxyz12345679".split("")
  private val shifted = "zyxwvutsrqponmlkjihgfedcba12345679"split("")

  val cipherMap = org.zip(shifted).toMap
}

case class Atbash() {
  import Atbash._

  def encode(in: String): String =
    in.toLowerCase
      .split("")
      .filter(cipherMap.contains(_))
      .map(cipherMap)
      .grouped(5)
      .map(_.mkString)
      .mkString(" ")
}
