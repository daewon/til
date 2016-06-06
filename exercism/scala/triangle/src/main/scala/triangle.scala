object TriangleType {
  trait TriangleTypeBase

  object Equilateral extends TriangleTypeBase
  object Isosceles extends TriangleTypeBase
  object Scalene extends TriangleTypeBase
  object Illogical extends TriangleTypeBase

  def matchTriangleType(a: Int, b: Int, c: Int) =
    if (a == b && b == c) Equilateral
    else if (a + b <= c) Illogical
    else if (b == c) Isosceles
    else Scalene
}

object Triangle {
  def apply(a: Int, b: Int, c: Int) = new Triangle(a: Int, b: Int, c: Int)
}

/**
  The sum of the lengths of any two sides of a triangle always exceeds the
  length of the third side, a principle known as the _triangle
  inequality_.
  **/
class Triangle(al: Int, bl: Int, cl: Int) {
  import TriangleType._

  def triangleType: TriangleTypeBase = List(al, bl, cl).sorted match {
    case a :: b :: c :: Nil => matchTriangleType(a, b, c)
  }
}
