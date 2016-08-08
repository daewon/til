object PascalsTriangle {
  def next(prev: Vector[Int]): Vector[Int] =
    (0 +: prev) zip (prev :+ 0) map { case (l, r) => l + r }

  val triangles = Stream.iterate(Vector(1))(next)

  def triangle(n: Int): Seq[Seq[Int]] = triangles take n
}
