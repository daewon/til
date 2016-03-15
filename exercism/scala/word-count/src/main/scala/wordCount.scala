class Phrase(sentence: String) {
  def wordCount = {
    val words = ("""(\w|')+""".r).findAllIn(sentence)

    words.foldLeft(Map.empty[String, Int]) { case (acc, word) =>
      val count = acc.getOrElse(word.toLowerCase, 0)
      acc + (word.toLowerCase -> (count + 1))
    }
  }
}
