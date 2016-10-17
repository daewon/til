object SumOfMultiples {
  def sumOfMultiples(ls: Seq[Int], limit: Int): Int =
    ls.flatMap { n =>
      def stream = Stream.iterate(n)(_ + n)
      stream.takeWhile(_ < limit).toList
    }.toSet.sum
}
