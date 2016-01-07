import scala.util.Random

def quick(arr: Array[Int], s: Int, e: Int): Array[Int] = {
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
      quick(arr, s, sIndex-1)
      quick(arr, sIndex+1, e)
    } else {
      if (sIndex == pivotIndex) {
        quick(arr, s, eIndex)
        quick(arr, eIndex + 1, e)
      } else if (eIndex == pivotIndex) {
        quick(arr, s, sIndex + 1)
        quick(arr, sIndex, e)
      } else {
        throw new IllegalStateException("cannot reach here.")
      }
    }
  }
  arr
}
//val array = Array(1, 7, 8, 9, 9, 9, 9, 2, 6, 4)
//val array = Array(5, 4, 9, 7, 3, 6, 0, 2, 8, 1, 10)
//val array = Array(0, 1, 4, 5, 6, 8, 3, 2, 7)
//val array = Array(6, 8, 2, 0, 3, 4, 1, 5, 7)
//quick(array, 0, array.length - 1)
(0 to 1000).map { n =>
  val rng = 0 to 5
  val randomArr = Random.shuffle(rng).toArray
  val org = randomArr.toList
  quick(randomArr, 0, randomArr.length - 1)
  val ret = randomArr.toList == rng.toList
  if (!ret) {
    println(s"$org => ${randomArr.toList}")
  }
  ret
}.forall(identity)
