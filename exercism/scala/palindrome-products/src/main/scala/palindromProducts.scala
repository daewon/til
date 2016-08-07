case class PalindromeProducts(a: Int, b: Int) {
  lazy val palindromies = for {
    i <- (a to b).toSet[Int]
    j <- i to b
    if isPalindrom(i * j)
  } yield (i, j, i * j)

  def isPalindrom(n: Int): Boolean = {
    val s = n.toString
    s.reverse == s
  }

  def factors(n: Int): Set[(Int, Int)] = palindromies
    .filter(n == _._3)
    .map { case (i, j, _) => (i, j) }

  def smallest: (Int, Set[(Int, Int)]) = {
    val min = palindromies.map(_._3).min

    min -> factors(min)
  }

  def largest: (Int, Set[(Int, Int)]) = {
    val max = palindromies.map(_._3).max

    max -> factors(max)
  }
}
