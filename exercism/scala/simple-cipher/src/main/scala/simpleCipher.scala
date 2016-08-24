case class Cipher(keyOpt: Option[String]) {
  val key = keyOpt.fold("aaaaaaaaaa") { str =>
    if (str.isEmpty || str.exists( c => c < 'a' || c > 'z')) throw new IllegalArgumentException
    str
  }

  val pivot = 'a'

  def rotate(diff: Int, ch: Char, shift: Char): Char = {
    val next = (ch - pivot + diff) % 26
    (pivot + next) toChar
  }

  def encode(in: String): String = in.zip(key).map { case (ch, shift) =>
    rotate(shift - pivot, ch, shift)
  }.mkString

  def decode(in: String): String = in.zip(key).map { case (ch, shift) =>
    rotate(pivot - shift, ch, shift)
  }.mkString
}
