import scala.collection.JavaConversions._

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution {
  object MajorityElement {

    case class Candidate(idx: Int, count: Int)

    def solve(arr: Array[Int]): Option[Candidate] = {
      val candidate = findCandidate(arr)

      if (isMajority(arr, candidate)) Option(candidate)
      else None
    }

    def isMajority(arr: Array[Int], candidate: Candidate): Boolean =
      arr.count(_ == arr(candidate.idx)) / 2

    def findCandidate(arr: Array[Int]): Candidate =
      arr.indices.foldLeft(Candidate(0, 1)) { (acc, idx) =>
        val newAcc =
          if (arr(acc.idx) == arr(idx)) acc.copy(count = acc.count + 1)
          else acc.copy(count = acc.count - 1)

        if (newAcc.count == 0) Candidate(idx, 1)
        else newAcc
      }
  }

  def solution(A: Array[Int]): Int = {
    MajorityElement.solve(A) match {
      case Some(acc) => acc.idx
      case None => -1
    }
  }
}
