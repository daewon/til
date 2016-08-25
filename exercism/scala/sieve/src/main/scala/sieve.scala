object Sieve {

  def primesUpTo(limit: Int): Vector[Int] = limit match {
    case 0 | 1 => Vector.empty
    case _ =>
      def updateCache(n: Int): Set[Int] = (n to limit by n).filter(_ % n == 0).toSet

      def upTo(primes: Vector[Int], cache: Set[Int]): Vector[Int] = {
        val nextPrimeOpt = ((primes.last) to limit).toStream.filterNot(cache).headOption
        nextPrimeOpt.fold(primes)(p => upTo(primes :+ p, cache | updateCache(p)))
      }

      upTo(Vector(2), updateCache(2))
  }
}
