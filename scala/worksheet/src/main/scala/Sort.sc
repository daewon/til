import scala.util.Random

def quickPoor(arr: Array[Int], s: Int, e: Int): Array[Int] = {
  def swap(i: Int, j: Int) = {
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  }
  val size = e - s + 1
  if (size <= 1) {
  } else if (size == 2) {
    if (arr(s) > arr(e)) swap(s, e)
  } else {
    val pivotIndex = s + (size / 2)
    val pivot = arr(pivotIndex)
    var sIndex = s
    var eIndex = e
    while (sIndex < pivotIndex && eIndex > pivotIndex) {
      while (arr(sIndex) <= pivot && sIndex < pivotIndex) sIndex += 1
      while (arr(eIndex) >= pivot && eIndex > pivotIndex) eIndex -= 1

      if (arr(sIndex) > arr(eIndex)) swap(sIndex, eIndex)
    }

    if (sIndex == eIndex) {
      quickPoor(arr, s, sIndex - 1)
      quickPoor(arr, sIndex + 1, e)
    } else quickPoor(arr, s, e)
  }
  arr
}
def quick(arr: Array[Int], _s: Int, _e: Int) = {
  def swap(s: Int, e: Int) {
    val t = arr(s)
    arr(s) = arr(e)
    arr(e) = t
  }
  def sortInner(s: Int, e: Int) {
    val pivot = arr((s + e) / 2)
    var sIndex = s
    var eIndex = e

    while (sIndex <= eIndex) {
      while (arr(sIndex) < pivot) sIndex += 1
      while (arr(eIndex) > pivot) eIndex -= 1

      if (sIndex <= eIndex) {
        swap(sIndex, eIndex)
        sIndex += 1
        eIndex -= 1
      }
    }

    if (s < eIndex) sortInner(s, eIndex)
    if (eIndex < e) sortInner(sIndex, e)
  }
  sortInner(0, arr.length - 1)
  arr
}


def duration(block: => Unit): Long = {
  val st = System.currentTimeMillis()
  block
  val duration = System.currentTimeMillis() - st
  duration
}

duration {
  (0 to 1000).map { n =>
    val rng = 0 to n
    val randomArr = Random.shuffle(rng).toArray
    val org = randomArr.toList
    quickPoor(randomArr, 0, randomArr.length - 1)
    val ret = randomArr.toList == rng.toList
    if (!ret) {
      println(s"$org => ${randomArr.toList}")
    }
    ret
  }.forall(identity)
}

duration {
  (0 to 1000).map { n =>
    val rng = 0 to n
    val randomArr = Random.shuffle(rng).toArray
    val org = randomArr.toList
    quick(randomArr, 0, randomArr.length - 1)
    val ret = randomArr.toList == rng.toList
    if (!ret) {
      println(s"$org => ${randomArr.toList}")
    }
    ret
  }.forall(identity)
}

