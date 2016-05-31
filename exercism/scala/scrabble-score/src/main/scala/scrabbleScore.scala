class Scrabble {
  val scoreText = Seq(
    "A, E, I, O, U, L, N, R, S, T" -> 1,
    "D, G" -> 2,
    "B, C, M, P" -> 3,
    "F, H, V, W, Y" -> 4,
    "K" -> 5,
    "J, X" -> 8,
    "Q, Z" -> 10
  )

  val score: Map[String, Int] = scoreText.flatMap { case (line, v) =>
    line.split(",").map(_.trim).map { ch => ch -> v }
  }.toMap

  def scoreLetter(s: Char): Int = score(s.toString.toUpperCase)
  def scoreWord(s: String): Int = s.map(c => score(c.toString.toUpperCase)).sum
}
