
def quick(xs: Array[Int]) = {
  def swap(i: Int, j: Int) {
    val t = xs(i)
    xs(i) = xs(j)
    xs(j) = t
  }
  def sort1(l: Int, r: Int) {
    val pivot = xs((l + r) / 2)
    var i = l
    var j = r
    while (i <= j) {
      while (xs(i) < pivot) i += 1
      while (xs(j) > pivot) j -= 1
      if (i <= j) {
        swap(i, j)
        i += 1
        j -= 1
      }
    }
    if (l < j) sort1(l, j)
    if (j < r) sort1(i, r)
  }
  sort1(0, xs.length - 1)
  xs
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
