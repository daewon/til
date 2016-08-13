object PythagoreanTriplet {
  type Triple = (Int, Int, Int)

  def isPythagorean(triple: Triple): Boolean = {
    val (_a, _b, _c) = triple
    val Seq(a, b, c) = Seq(_a, _b, _c).sorted

    (a * a) + (b * b) == c * c
  }

  def pythagoreanTriplets(from: Int, to: Int): Seq[Triple] = for {
    a <- from to to
    b <- a to to
    c <- b to to
    if isPythagorean((a, b, c))
  } yield (a, b, c)
}
