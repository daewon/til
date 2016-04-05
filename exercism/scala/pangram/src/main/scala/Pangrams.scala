object Pangrams {
  def isPangram(arg: String) = {
    val nomalized = arg.map(Character.toUpperCase).filter(_.toString.matches("[A-Z]"))
    val map = nomalized.foldLeft(Map.empty[Char, Int]) { case (acc, ch) =>
      acc + (ch -> (acc.getOrElse(ch, 0) + 1))
    }

    map.size == 26
  }
}
