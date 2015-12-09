import scala.collection.mutable.ArrayBuffer

// http://docs.scala-lang.org/overviews/core/architecture-of-scala-collections.html
// http://www.scala-lang.org/docu/files/collections-api/collections-impl.html

val buf = new ArrayBuffer[Int]
buf += 1
val a = buf mapResult { _.toList }

