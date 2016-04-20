object Pangrams {
  def isUpperAlpha(ch: Char): Boolean = ('A' to 'Z').toSet(ch)
  def isPangram(sentence: String) =
    sentence
      .toUpperCase
      .toSet
      .filter(isUpperAlpha)
      .size == 26
}
