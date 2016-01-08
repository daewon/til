
def quick(arr: Array[Int]) = {
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
// List(5, 3, 2, 4, 6, 1, 0) == List(0, 1, 2, 3, 4, 6, 5)
val array = Array(5, 3, 2, 4, 6, 1, 0)
quick(array)
//(0 to 1000).map { n =>
//  val rng = 0 to 6
//  val randomArr = Random.shuffle(rng).toArray
//  val org = randomArr.toList
//  quick(randomArr, 0, randomArr.length - 1)
//  val ret = randomArr.toList == rng.toList
//  if (!ret) {
//    println(s"$org => ${randomArr.toList}")
//  }
//  ret
//}.forall(identity)
