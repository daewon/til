object MajorityElement {

  case class Candidate(idx: Int, count: Int)

  def solve(arr: Array[Int]): Option[Int] = {
    val candidate = findCandidate(arr)

    if (isMajority(arr, candidate)) Option(arr(candidate.idx))
    else None
  }

  def isMajority(arr: Array[Int], candidate: Candidate) =
    arr.count(_ == arr(candidate.idx)) > arr.size / 2

  def findCandidate(arr: Array[Int]): Candidate =
    arr.indices.foldLeft(Candidate(0, 1)) { (acc, idx) =>
      val newAcc =
        if (arr(acc.idx) == arr(idx)) acc.copy(count = acc.count + 1)
        else acc.copy(count = acc.count - 1)

      if (newAcc.count == 0) Candidate(idx, 1)
      else newAcc
    }
}

val arr = Array(1, 1, 1, 2, 3, 4, 1)
val ret = MajorityElement.solve(arr)

ret match {
  case Some(n) => println(s"Found Majority Element: $n")
  case None => println("No Majority Element")
}
