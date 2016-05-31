case class Squares() {
  def sumOfSquares(n: Int): Int =
    1 to n map { n => Math.pow(n, 2).toInt } sum

  def squareOfSums(n: Int): Int = Math.pow(1 to n sum, 2).toInt

  def difference(n: Int): Int = squareOfSums(n) - sumOfSquares(n)
}
