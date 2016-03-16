class Anagram(org: String) {
  private def toMap(s: String) = s.foldLeft(Map.empty[Char, Int]) { case (acc, curr) =>
    val cnt = acc.getOrElse(curr, 0)
    acc + (curr -> (cnt + 1))
  }

  private val originalMap = toMap(org.toLowerCase)
  def matches(ls: Seq[String]) = ls
    .filter(_.toLowerCase != org.toLowerCase)
    .filter { word => toMap(word.toLowerCase) == originalMap
  }
}
