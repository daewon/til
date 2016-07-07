trait Show[A] {
  def show(e: A): String
}

object Show {
  implicit val intShow = new Show[Int] {
    override def show(n: Int): String = n.toString
  }
}

def toShow[T: Show](a: T): Show[T] = {
  a
}

val a = toShow(123)
