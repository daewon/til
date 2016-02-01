
trait SemiGroup[A] {
  def add(a: A, b: A): A
}

trait Monoid[A] extends SemiGroup[A] {
  def unit: A
}

implicit object StringMonoid extends Monoid[String] {
  override def add(a: String, b: String): String = a + b

  override def unit: String = ""
}

implicit object IntMonoid extends Monoid[Int] {
  override def add(a: Int, b: Int): Int = a + b

  override def unit: Int = 0
}

def sum[A: Monoid](xs: List[A]): A = xs match {
  case hd :: tl => implicitly[Monoid[A]].add(hd, sum(tl))
  case Nil => implicitly[Monoid[A]].unit
}

