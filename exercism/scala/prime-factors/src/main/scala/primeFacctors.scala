object PrimeFactors {
  def apply() = new PrimeFactors()
}

class PrimeFactors() {

  def primeFactors(n: Long): Seq[Int] = {
    var totalNum = n
    var prime = 2
    var acc: Vector[Int] = Vector.empty

    while (totalNum > 1) {
      if (totalNum % prime == 0) {
        acc = acc :+ prime
        totalNum = totalNum / prime
      } else {
        prime = if (prime == 2) prime + 1 else prime + 2
      }
    }

    acc
  }
}
