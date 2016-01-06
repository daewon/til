def swap(arr: Array[Int], i: Int, j: Int) = {
  val tmp = arr(i)
  arr(i) = arr(j)
  arr(j) = arr(tmp)
}
def quick(arr: Array[Int], s: Int, e: Int): Array[Int] = {
  val size = e - s

  if (size < 1) {
  } else if (size == 2) {
    if (arr(s) > arr(e)) swap(arr, s, e)
  } else {
    val pivotIndex = size
    val pivot = arr(pivotIndex)

    var sIndex = s
    var eIndex = e
    while (sIndex != eIndex) {

      while (arr(sIndex) > pivot && sIndex < pivotIndex) {
        sIndex += 1
      }

      while (arr(eIndex) < pivot && eIndex > pivotIndex) {
        eIndex -= 1
      }
      println(eIndex)
    }
  }

  arr
}
val array = Array(1, 7, 8, 9, 2, 6, 4)
quick(array, 0, array.length - 1)