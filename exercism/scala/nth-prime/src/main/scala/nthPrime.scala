object Prime {
  def prime(limit: Int): Int = {
    val step = 2
    @scala.annotation.tailrec
    def find(count: Int, n: Int, primes: Set[Int]): Int =
      if (count == limit) n - step
      else {
        if (primes.forall(p => n % p != 0)) find(count + 1, n + step, primes + n)
        else find(count, n + step, primes)
      }

    find(0, 3, Set.empty[Int])
  }

  def nth(n: Int): Int = n match {
    case 1 => 2
    case _ => prime(n-1)
  }
}
