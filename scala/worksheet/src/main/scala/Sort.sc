import scala.util.Random

def quick(arr: Array[Int]): Array[Int] = {
  def swap(a: Int, b: Int) = {
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
