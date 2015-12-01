import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import language.higherKinds // so that Scala 2.10 doesn't warn
import scala.collection.JavaConversions._
/*
 http://www.michaelpollmeier.com/create-generic-scala-collections-with-canbuildfrom
 */
def map[A, B, C[A] <: Iterable[A]](iterable: C[A])
                                  (f: A => B)
                                  (implicit cbf: CanBuildFrom[C[A], B, C[B]]): C[B] = {
  val builder = cbf(iterable)
  builder.sizeHint(iterable.size)
  iterable foreach { e => builder += f(e) }
  builder.result
}
map(Set(1, 2))(_.toString) //returns Set[String]
map(List(1, 2))(_.toString) //returns List[String]
def map2[A, B, C[A] <: Iterable[A], D[_]](iterable: C[A])
                                         (f: A => B)
                                         (implicit cbf: CanBuildFrom[Nothing, B, D[B]]): D[B] = {
  val builder = cbf()
  builder.sizeHint(iterable.size)
  iterable.foreach { e => builder += f(e) }
  builder.result()
}
map2(Set(1, 2))(_.toString)(new CanBuildFrom[Nothing, String, Set[String]] {
  override def apply(from: Nothing): mutable.Builder[String, Set[String]] = ???
  override def apply(): mutable.Builder[String, Set[String]] = {
    new mutable.Builder[String, Set[String]] {
      var collection = Set[String]()

      override def +=(elem: String): this.type = { collection += elem ; this }
      override def result(): Set[String] = Set()
      override def clear(): Unit = collection = Set()
    }
  }
})
/*
 http://blog.bruchez.name/2012/08/getting-to-know-canbuildfrom-without-phd.html
 */
def combineValues(pairs: Seq[(String, String)]): Seq[(String, Seq[String])] = {
  val result = mutable.LinkedHashMap[String, List[String]]()
  for ((name, value) <- pairs)
    result += name -> (value :: result.getOrElse(name, Nil))
  result.toList
}
combineValues(Seq("daewon" -> "30", "daewon" -> "40", "dun" -> "1"))
def combineValues2[U, T[_]](pairs : Seq[(String, U)])
                           (implicit cbf: CanBuildFrom[_, U, T[U]]): Iterable[T[U]] = {
  val result = mutable.LinkedHashMap[String, mutable.Builder[U, T[U]]]()
  for ((name, value) <- pairs)
    result.getOrElseUpdate(name, cbf()) += value
  result.values.map(_.result)
}
combineValues2(Seq("daewon" -> "30", "daewon" -> "40", "dun" -> "1"))
combineValues2(List("daewon" -> 30, "daewon" -> 40, "dun" -> 1))
combineValues2(Vector("daewon" -> "30", "daewon" -> "40", "dun" -> "1"))
val ls = Seq("daewon" -> "30", "daewon" -> "40", "dun" -> "1")
val javaList = new java.util.ArrayList[(String, String)]()
ls.foreach { e => javaList.add(e) }
combineValues2(javaList)
