import scala.language.implicitConversions

/**
http://michalostruszka.pl/blog/2015/03/30/scala-case-classes-to-and-from-tuples/
  * */

case object My

def typeBound[A: ({type E[_] = String with Int <:< A})#E](arg: A): A = arg
def typeBound2[A](arg: A)(implicit ev: String with Int <:< A): A = arg
def typeBound3[A](arg: A)(implicit ev: String with Int with My.type <:< A): A = arg
typeBound(1)
typeBound("string")
// typeBound(0.1f) // error on compile

typeBound2(1)
typeBound2("string")
// typeBound2(0.1f) // error on compile

typeBound3("string")
typeBound3(1)
typeBound3(My)
// typeBound3(0.1f) // error on compile

/**
http://jatinpuri.com/2014/03/replace-view-bounds/
  * */

// view bounds @deprecated
def foo[T <% Int](x: T): Int = x
implicit def convert[T](n: T) = n match {
  case x: String => x.toInt
}
foo("100") // error if convert implicit function was not presented

// context bounds
type L[X] = X => Int
def foo2[T: L](n: T): Int = n
foo2("100")
// same as above
def foo3[T: ({type L[X] = X => Int})#L](n: T) = n
foo3("100")
// same as above
def foo4[T](n: T)(implicit ev: T => Int): Int = n
// generalized
def goo[T, E](x: T)(implicit ev: T => E): (T, E) = {
  val converted = ev(x)
  (x, converted)
}
goo("100")((x: String) => x.toInt)
goo(100)
def goo2[T, E: ({type L[X] = T => E})#L](x: T): (T, E) = {
  val converted = implicitly(x)
  (x, converted)
}
goo2("100")(Seq(_))
goo2(100)

