var binArr = new Array[Int](1000)
(0 until 100) foreach { n =>
  binArr(n) = scala.util.Random.nextInt(2)
}

var i = 0
var j = binArr.length - 1

while (i <= j) {
  while (binArr(i) == 0) i += 1
  while (binArr(j) == 1) j -= 1

  val tmp = binArr(i)
  binArr(i) = binArr(j)
  binArr(j) = tmp
}

binArr

