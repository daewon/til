trait Semigroup[A] {
  def append(a: A, b: A): A
}

object Semigroup {
  implicit val StringSemigroup = new Semigroup[String] {
    def append(a: String, b: String) = a + b
  }
}

trait Monoid[A] extends Semigroup[A] {
  def zero: A
}

object Monoid {
  implicit def optionMonoid[A](implicit ev: Semigroup[A]) = new Monoid[Option[A]] {
    def zero : Option[A] = None
    def append(a : Option[A], b: Option[A]) : Option[A] =
      (a, b) match {
        case (Some(a1), Some(b1)) => Some(ev.append(a1, b1))
        case (Some(_), _) => a
        case (_, Some(_)) => b
        case _ => zero
      }
  }
}

case class Raindrops() {
  import Monoid._

  def convert(n: Int)(implicit ev: Monoid[Option[String]]): String = {
    val pling = if (n % 3 == 0) Option("Pling") else None
    val plang = if (n % 5 == 0) Option("Plang") else None
    val plong = if (n % 7 == 0) Option("Plong") else None

    Seq(pling, plang, plong).reduce[Option[String]](ev.append).getOrElse(n.toString)
  }
}
