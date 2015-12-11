import scala.collection._

class PrefixMap[T](private val map: mutable.Map[Char, PrefixMap[T]],
                   val value: Option[T] = None) {
  def apply(ch: Char) = map(ch)

  def apply(str: String): Option[T] = {
    var currentMap = this
    str.foreach { ch =>
      currentMap = currentMap(ch)
    }

    currentMap.value
  }

  override def toString() = map.toString
}

object PrefixMap {
  def empty[T] = new PrefixMap[T](mutable.Map.empty)

  def withValue[T](value: T) = new PrefixMap[T](mutable.Map.empty, Option(value))

  def build[T](kv: (String, T), parent: PrefixMap[T]): PrefixMap[T] = {
    val (key, value) = kv

    if (key.nonEmpty) {
      if (!parent.map.contains(key.head)) {
        if (key.length == 1) parent.map.update(key.head, PrefixMap.withValue(value))
        else parent.map.update(key.head, PrefixMap.empty)
      }

      build(key.tail -> value, parent(key.head))
    }

    parent
  }

  def fromSeq[T](kvs: (String, T)*): PrefixMap[T] = {
    if (kvs.nonEmpty) {
      val root = build(kvs.head, PrefixMap.empty)
      kvs.tail.foreach { case (k, v) =>
        build(k -> v, root)
      }
      root
    } else PrefixMap.empty
  }
}

//PrefixMap.fromSeq("ABC" -> true, "AX" -> true)
val m = PrefixMap.fromSeq("abc" -> true, "abcd" -> true, "al" -> true, "all" -> true, "xy" -> true)

val x = m('x')
val y = m("ab")
