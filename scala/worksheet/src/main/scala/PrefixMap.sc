import scala.collection._
import scala.collection.generic.CanBuildFrom
object PrefixMap {
  def empty[T] = new PrefixMap[T](mutable.Map.empty)
  def withValue[T](value: T) = new PrefixMap[T](mutable.Map.empty, Option(value))
  def newBuilder[T] = new mutable.Builder[(String, T), PrefixMap[T]] {
    var prefixMap = PrefixMap.empty[T]
    override def +=(elem: (String, T)): this.type = {
      prefixMap += elem
      this
    }
    override def result(): PrefixMap[T] = prefixMap
    override def clear(): Unit = prefixMap = PrefixMap.empty[T]
  }
  implicit def prefixMapCanBuildFrom[T] = new CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] {
    override def apply(from: PrefixMap[_]): mutable.Builder[(String, T), PrefixMap[T]] = newBuilder
    override def apply(): mutable.Builder[(String, T), PrefixMap[T]] = newBuilder
  }
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

  def apply[T](kvs: (String, T)*): PrefixMap[T] = {
    val root = PrefixMap.empty[T]
    kvs.foreach { case (k, v) =>
      build(k -> v, root)
    }
    root
  }
}

class PrefixMap[T](var map: mutable.Map[Char, PrefixMap[T]],
                   var value: Option[T] = None)
  extends mutable.Map[String, T]
  with mutable.MapLike[String, T, PrefixMap[T]] {
  self =>

  override def empty = PrefixMap.empty[T]

  override def newBuilder = PrefixMap.newBuilder[T]

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

  override def remove(key: String): Option[T] = {
    var value = None: Option[T]
    def traverse(str: String, parent: PrefixMap[T]): Unit = {
      if (str.nonEmpty) {
        if (parent.map.contains(str.head)) {
          traverse(str.tail, parent(str.head))
          if (str.length == 1) {
            value = parent(str.head).value
            parent(str.head).value = None
          }
        } else throw new RuntimeException("key not found")

        if (parent(str.head).map.isEmpty && parent(str.head).value.isEmpty) parent.map.remove(str.head)
      }
    }

    traverse(key, this)

    value
  }

  override def update(key: String, value: T): Unit = {
    PrefixMap.build(key -> value, this)
  }

  override def +=(kv: (String, T)): this.type = {
    val (k, v) = kv
    update(k, v)
    this
  }

  override def -=(key: String): this.type = {
    remove(key)
    this
  }

  def iterator2: Iterator[(String, T)] = new Iterator[(String, T)] {
    val current = value.iterator.map("" -> _)
    val subs = for {
      (k, sub) <- self.map.iterator
      (c, v) <- sub.iterator
    } yield (k + c) -> v

    val iter = current ++ subs

    override def hasNext: Boolean = iter.hasNext

    override def next(): (String, T) = iter.next
  }

  override def iterator: Iterator[(String, T)] = new Iterator[(String, T)] {
    lazy val kvsIterator = {
      var kvs = Vector.empty[(String, T)]
      def traverse(prefixMap: PrefixMap[T], stack: Vector[Char]): Unit = {
        prefixMap.map.keys.foreach { ch =>
          traverse(prefixMap.map(ch), stack :+ ch)
        }

        if (prefixMap.value.isDefined) kvs = (stack.mkString, prefixMap.value.get) +: kvs
      }

      traverse(self, Vector.empty)
      kvs
    }

    lazy val it = kvsIterator.iterator

    override def hasNext: Boolean = it.hasNext

    override def next(): (String, T) = it.next
  }

  override def toString = map.toString

  def toString2 = {
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

PrefixMap("ABC" -> true, "AX" -> true)
val m = PrefixMap("abc" -> true, "abcd" -> true, "al" -> true, "all" -> true, "xy" -> true)
val newMap = m.map { kv =>
  kv._1 -> false
} // build with CanBuildFrom
val newMap2 = m.filter { case (k, v) =>
    k.contains("abc")
  } // build with builder
val x = m('x')
val y = m.get("ab")
m += "abc" -> false
m += "zzzz" -> true
m -= "abc"
val z = m.get("abc")
val k = m.get("abcd")
// test grabege collected
// 1. PrefixMap with (abc, abed)
// 2. Remove abc
// 3. map must removed 4depth('d') just have 3depth element('abc')
val m2 = PrefixMap("abc" -> true, "abcd" -> true)
m2.get("abcd")
m2.get("abc")
m2.remove("abcd")
m2.get("abc")
m2.get("abcd")
m2
