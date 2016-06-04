object Dna {
  val RnaMap = Map("G" -> "C", "C" -> "G", "T" -> "A", "A" -> "U")

  def apply() = new Dna()
}

class Dna() {
  def toRna(s: String): String =
    s.split("")
      .map(Dna.RnaMap.getOrElse(_, ""))
      .mkString("")
}
