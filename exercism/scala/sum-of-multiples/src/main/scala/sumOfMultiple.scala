object SumOfMultiples {
  def sumOfMultiples(ls: Seq[Int], limit: Int): Int =
    ls.flatMap { n => n until limit by n }.toSet sum
}
