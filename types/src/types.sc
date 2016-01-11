import scala.language.implicitConversions

//http://michalostruszka.pl/blog/2015/03/30/scala-case-classes-to-and-from-tuples/
//http://jatinpuri.com/2014/03/replace-view-bounds/

//def typeBound[A](arg: A)(implicit ev: String with Int <:< A): A = arg
def typeBound[A: ({type E[_] = String with Int <:< A})#E](arg: A): A = arg
typeBound(1)
typeBound("string")
//typeBound(1.0) // error
