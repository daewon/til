import scala.collection._

case class PrefixMap[T](map: Map[Char, PrefixMap[T]]) {
  def apply(ch: Char) = map(ch)

  override def toString() = map.toString
}

object PrefixMap {
  def empty[T] = new PrefixMap[T](Map.empty)

  def build[T](kv: (String, T), parent: PrefixMap[T]): PrefixMap[T] = {
    val (key, value) = kv

    if (key.isEmpty) parent
    else {
      val updated = parent.copy(map = parent.map.updated(key.head, PrefixMap.empty))
      build((key.tail, value), updated(key.head))
    }
  }

  def fromSeq[T](kvs: (String, T)*): PrefixMap[T] = {
    build(kvs.head, PrefixMap.empty)
  }
}
//PrefixMap.fromSeq("ABC" -> true, "AX" -> true)
PrefixMap.fromSeq("ABC" -> true)
