object PythagoreanTriplet {
  type Triple = (Int, Int, Int)

  def tripleToList(t: Triple) = t.productIterator.toList.asInstanceOf[Seq[Int]]

  def isPythagorean(triple: Triple): Boolean = {
    val a :: b :: c :: Nil = tripleToList(triple).sorted
    (a * a) + (b * b) == c * c
  }

  def pythagoreanTriplets(from: Int, to: Int): Seq[Triple] = for {
    a <- from to to
    b <- a to to
    c <- b to to
    if isPythagorean((a, b, c))
  } yield (a, b, c)
}
