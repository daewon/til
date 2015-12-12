import scala.collection._

object PrefixMap {
  def empty[T] = new PrefixMap[T](mutable.Map.empty)

  def withValue[T](value: T) = new PrefixMap[T](mutable.Map.empty, Option(value))

  def build[T](kv: (String, T), parent: PrefixMap[T]): PrefixMap[T] = {
    val (key, value) = kv
    if (key.nonEmpty) {
      if (parent.map.contains(key.head)) {
        if (key.length == 1) parent(key.head).value = Option(value)
      } else {
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

class PrefixMap[T](private val map: mutable.Map[Char, PrefixMap[T]],
                   var value: Option[T] = None) {
  def apply(ch: Char) = map(ch)

  def get(str: String): Option[T] = {
    var currentMap = this
    var isFail = false

    str.foreach { ch =>
      if (currentMap.map.contains(ch)) currentMap = currentMap(ch)
      else {
        isFail = true
      }
    }

    if (isFail) None
    else currentMap.value
  }

  def remove(key: String): this.type = {
    def traverse(str: String, parent: PrefixMap[T]): Unit = {
      if (str.nonEmpty) {
        if (parent.map.contains(str.head)) {
          traverse(str.tail, parent(str.head))
          if (str.length == 1) parent(str.head).value = None
        } else throw new RuntimeException("key not found")

        if (parent(str.head).map.isEmpty && parent(str.head).value.isEmpty) parent.map.remove(str.head)
      }
    }

    traverse(key, this)

    this
  }

  def update(key: String, value: T): this.type = {
    PrefixMap.build(key -> value, this)
    this
  }

  override def toString() = {
    //    map.toString
    var kvs = Vector.empty[(String, T)]
    def traverse(prefixMap: PrefixMap[T], stack: Vector[Char]): Unit = {
      prefixMap.map.keys.foreach { ch =>
        traverse(prefixMap.map(ch), stack :+ ch)
      }
      if (prefixMap.value.isDefined) kvs = (stack.mkString, prefixMap.value.get) +: kvs
    }
    traverse(this, Vector.empty)
    kvs.map { case (k, v) => s"$k -> $v" } mkString("PrefixMap(", ", ", ")")
  }
}


PrefixMap.fromSeq("ABC" -> true, "AX" -> true)
val m = PrefixMap.fromSeq("abc" -> true, "abcd" -> true, "al" -> true, "all" -> true, "xy" -> true)
val x = m('x')
val y = m.get("ab")
m.update("abc", false)
m.update("zzzz", true)
m.remove("abc")
val z = m.get("abc")
val k = m.get("abcd")
//Map("abc" -> true)

//val m = PrefixMap.fromSeq("abc" -> true, "abcd" -> true)
//m.get("abcd")
//m.get("abc")
//
//m.remove("abcd")
//m.get("abc")
//m.get("abcd")
