import scala.collection._
import scala.collection.generic.CanBuildFrom

object PrefixMap extends {
  def empty[T] = new PrefixMap[T]

  def apply[T](kvs: (String, T)*): PrefixMap[T] = {
    val m: PrefixMap[T] = empty
    for (kv <- kvs) m += kv
    m
  }

  def newBuilder[T]: mutable.Builder[(String, T), PrefixMap[T]] =
    new mutable.MapBuilder[String, T, PrefixMap[T]](empty)

  implicit def canBuildFrom[T]: CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] =
    new CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] {
      def apply(from: PrefixMap[_]) = newBuilder[T]

      def apply() = newBuilder[T]
    }
}

class PrefixMap[T]
  extends mutable.Map[String, T]
  with mutable.MapLike[String, T, PrefixMap[T]] {

  override def empty = new PrefixMap[T]()

  var suffixes: immutable.Map[Char, PrefixMap[T]] = immutable.Map.empty
  var value: Option[T] = None

  def get(s: String): Option[T] =
    if (s.isEmpty) value
    else suffixes.get(s.head).flatMap { m =>
      m.get(s.substring(1))
    }

  def withPrefix(s: String): PrefixMap[T] = {
    if (s.isEmpty) this
    else {
      val leading = s.head
      suffixes.get(leading) match {
        case None => suffixes += leading -> empty
        case _ =>
      }

      suffixes(leading).withPrefix(s.substring(1))
    }
  }

  override def +=(kv: (String, T)): this.type = {
    withPrefix(kv._1).value = Option(kv._2)

    this
  }

  override def -=(key: String): this.type = {
    if (key.isEmpty) value = None
    else suffixes.get(key.head).flatMap(_.remove(key.substring(1)))

    this
  }

  override def iterator: Iterator[(String, T)] = {
    val valueIt = for (v <- value.iterator) yield ("", v)
    val restIt = for {
      (chr, m) <- suffixes.iterator
      (s, v) <- m.iterator
    } yield (chr +: s, v)

    valueIt ++ restIt
  }

  // Can't clean_up garbage map
  //  override def toString() = suffixes.toString

//  override def toString = suffixes.toString
}

PrefixMap("ABC" -> true, "AX" -> true)
val m = PrefixMap("abc" -> true, "abcd" -> true, "al" -> true, "all" -> true, "xy" -> true)
val newMap = m.map { kv =>
  kv._1 -> false
} // build with CanBuildFrom
val newMap2 = m.filter { case (k, v) =>
    k.contains("abc")
  } // build with builder

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