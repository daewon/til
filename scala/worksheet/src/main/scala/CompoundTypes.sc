/**
http://docs.scala-lang.org/ko/tutorials/tour/compound-types.html
  */

trait Clonable extends java.lang.Cloneable {
  override def clone: Clonable = {
    super.clone.asInstanceOf[Clonable]
  }
}

trait Resetable {
  def reset(): Unit = { println("reset") }
}


def cloneAndReset(obj: Clonable with Resetable): Clonable = {
  val cloned = obj.clone()
  obj.reset()
  cloned
}

object resetWithClone extends Resetable with Clonable
cloneAndReset(resetWithClone)
/**
  * implicit ev: String with Int <:< A) means
  * A must Super type of String with Int
  * It work, because implicit ec with function `identity`
  */
def stringWithInt[A](obj: A)(implicit ev: String with Int <:< A) = {
 obj match {
   case a: String => println("string") ; a
   case n: Int => println("int") ; n
   case _ => println("any") ; obj
 }
}

stringWithInt(1)
stringWithInt("str")
//stringWithInt(3.0f)
