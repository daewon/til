class DNA(dna: String) {

  val base = Map('A' -> 0, 'T' -> 0, 'C' -> 0, 'G' -> 0)

  val map = dna.foldLeft(base) { case (acc, ch) =>
    acc.get(ch) match {
      case None => throw new RuntimeException("Invalid char")
      case Some(cnt) => acc + (ch -> (cnt + 1))
    }
  }

  def nucleotideCounts(): Map[Char, Int] = map

  def nucleotideCounts(ch: Char): Int = map(ch)
}
