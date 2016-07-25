trait Cache[K, V] {
  def evict(): Unit
  def add(key: K, value: V): Unit
  def get(key: K): V
  def remove(key: K)
}

object LazyEvictCache {
  case class CacheKey[K](key: K, id: Int = 0)
  case class CacheValue[V](value: V, id: Int = 0) {
    def inc = this.copy(id = id + 1)
  }
}

class LazyEvictCache[K, V] extends Cache[K, V] {
  import LazyEvictCache._

  // key, value map
  var map = Map.empty[CacheKey[K], CacheValue[V]]

  // sorted by key added time
  var keys: List[CacheKey[K]] = Nil

  def add(key: K, value: V): Unit = {
    val cacheKey = CacheKey(key)
    if (map.contains(cacheKey)) {
      // update old value id
      map += (cacheKey -> map(cacheKey).inc)
    } else {
      map += (cacheKey -> CacheValue(value))
    }

    keys = cacheKey :: keys
  }

  def get(key: K): V = {
    map(CacheKey(key)).value
  }

  def remove(key: K) = {
    map -= CacheKey(key)
  }

  def evict(): Unit = {
    val remains = keys.dropWhile { key =>
      !map.contains(key) || (map.contains(key) && key.id != map(key).id)
    }

    keys = remains match {
      case Nil => Nil
      case key :: tl =>
        map -= key
        tl
    }
  }
}

object Hello {
  def main(args: Array[String]) {
    val cache = new LazyEvictCache[Int, Int]()
    var ok = true
    var ls: Vector[String] = Vector.empty

    while (ok) {
      val ln = readLine()
      ls = ls :+ ln
      ok = ln != null
    }

    ls.foreach { line =>
      if (line.contains("add")) {
        val Array(cmd, key, value) = line.split(" ")
        cache.add(key.toInt, value.toInt)
      } else if (line.contains("get")) {
        val Array(cmd, key) = line.split(" ")
        val v = try cache.get(key.toInt) catch { case e: Exception => -1 }
        println(v)
      } else if (line.contains("evict")) {
        cache.evict()
      } else if (line.contains("remove")) {
        val Array(cmd, key) = line.split(" ")
        cache.remove(key.toInt)
      } else {
        // exit
      }
    }
  }
}
