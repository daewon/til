import scala.util.Random
import scala.collection._

def quick(arr: Array[Int]): Array[Int] = {
  def swap(a: Int, b: Int) = if (a != b) {
    val tmp = arr(a)
    arr(a) = arr(b)
    arr(b) = tmp
  }

  def quickInner(s: Int, e: Int): Unit = {
    var sIdx = s
    var eIdx = e
    val size = (eIdx - sIdx) + 1

    if (size <= 2) {
      if (arr(s) > arr(e)) swap(s, e)
    } else {
      val pivot = arr(s + (size / 2))

      while (sIdx <= eIdx) {
        while (arr(sIdx) < pivot) sIdx += 1
        while (arr(eIdx) > pivot) eIdx -= 1

        if (sIdx <= eIdx) {
          swap(sIdx, eIdx)

          sIdx += 1
          eIdx -= 1
        }
      }

      if (eIdx > s) quickInner(s, eIdx)
      if (sIdx < e) quickInner(sIdx, e)
    }
  }

  quickInner(0, arr.length - 1)
  arr
}

val arr = Array(2, 4, 0, 3, 9, 1, 7)
quick(arr)

(0 to 10000).map { n =>
  val rng = 0 to n
  val randomArr = Random.shuffle(rng).toArray
  val org = randomArr.toList
  quick(randomArr)
  val ret = randomArr.toList == rng.toList
  if (!ret) {
    println(s"$org => ${randomArr.toList}")
  }
  ret

}.forall(identity)


trait Test[+A] {
  def show[B >: A](a: B): String = a.toString
}

trait Ordered[T] {
  def lessThan(left: T, right: T): Boolean
}

class Sort[T] {
  val insertion = (arr: mutable.ArrayBuffer[T]) => { implicit ev: Ordered[T] =>
    var i = 1
    while (i < arr.size) {
      val a = arr(i)
      var find = false
      var j = 0

      while (j < i && !find) {
        var b = arr(j)
        if (ev.lessThan(a, b)) {
          // shifting
          var k = i
          while (k > j) {
            arr(k) = arr(k - 1)
            k -= 1
          }

          // swap
          arr(k) = a
          find = true
        }

        j += 1
      }

      i += 1
    }
  }

  val selection = (arr: mutable.ArrayBuffer[T]) => { implicit ev: Ordered[T] =>
    var i = 0
    while (i < arr.size - 1) {
      var minIndex = i
      var j = i + 1

      while (j < arr.size) {
        if (ev.lessThan(arr(j), arr(minIndex))) {
          minIndex = j
        }
        j += 1
      }

      val tmp = arr(minIndex)
      arr(minIndex) = arr(i)
      arr(i) = tmp

      i += 1
    }
  }

  def bubble[T: Ordered](arr: mutable.ArrayBuffer[T]) = {
    var i = 0
    var loop = true

    while (loop) {
      var j = i + 1
      var isChanged = false
      while (j < arr.size) {
        if (implicitly[Ordered[T]].lessThan(arr(j), arr(i))) {
          val tmp = arr(i)
          arr(i) = arr(j)
          arr(j) = tmp
          isChanged = true
        }
        j += 1
      }

      if (!isChanged) {
        loop = false
      }
      i += 1
    }
  }
}

implicit val IntOrdered = new Ordered[Int] {
  def lessThan(left: Int, right: Int) = left < right
}

val arr2 = mutable.ArrayBuffer(0, 1, 3, 1, 5, 0, 2, 1, 0)
val sort = new Sort[Int].insertion(arr2)(IntOrdered)

