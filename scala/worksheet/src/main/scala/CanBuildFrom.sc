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

case class MyContainer[T](ls: List[T] = Nil) {
  def +=(other: T) = copy(ls = other :: ls)
}
map2(Set(1, 2))(_.toString)(new CanBuildFrom[Nothing, String, MyContainer[String]] {
  override def apply(from: Nothing): mutable.Builder[String, MyContainer[String]] = apply()
  override def apply(): mutable.Builder[String, MyContainer[String]] = new mutable.Builder[String, MyContainer[String]] {
    var coll = MyContainer[String]()

    override def +=(elem: String): this.type = { coll = coll += elem ; this }
    override def result(): MyContainer[String] = coll
    override def clear(): Unit = coll = MyContainer()
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
/*
Mock CanBuild
 */
trait CanBuild[A, Container[_]] {
  def make(): Container[A]
  def append(container: Container[A], item: A)
}
trait MIterable[A, Container[_]] { self =>
  def foreach(f: A => Unit)

  def map[B](f: A => B)(implicit ev: CanBuild[B, Container]): Container[B] = {
    val newCont = ev.make()
    self.foreach { a =>
      val b = f(a)
      ev.append(newCont, b)
    }

    newCont
  }
}

class MList[T] extends MIterable[T, MList] {
  var items = List.empty[T]

  override def foreach(f: T => Unit): Unit = items.foreach(f)

  def add(item: T): MList[T] = {
    items = item :: items
    this
  }

  override def toString() = items.toString
}

implicit def MListCanBuild[T] = new CanBuild[T, MList] {
  override def make(): MList[T] = new MList[T]
  override def append(container: MList[T], item: T): Unit = {
    container.add(item)
  }
}

val intList = new MList[Int]
intList.add(1).add(2).add(3)
val stringList = intList.map { (i: Int) => (i * 2 ).toString }
