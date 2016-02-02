import scala.collection.JavaConversions._
import scala.annotation._

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution {
  def solution(A: Array[Int]): Int = {
    if (A.isEmpty) return 0

    // write your code in Scala 2.10
    @tailrec
    def merge(a: List[Int], b: List[Int], acc: List[Int] = Nil): List[Int] = (a, b) match {
      case (Nil, Nil) => acc.reverse
      case (Nil, _) => acc.reverse ::: b
      case (_, Nil) => acc.reverse ::: a
      case (ah :: atl, bh :: btl) =>
        if (ah < bh) merge(atl, b, ah :: acc) else merge(a, btl, bh :: acc)
    }

    def split(a: Seq[Int]) = a.splitAt(a.length / 2)

    def mergeSort(arr: Seq[Int]): List[Int] = {
      if (arr.length == 0) Nil
      else if (arr.length == 1) arr.toList
      else {
        val (a, b) = split(arr)
        merge(mergeSort(a), mergeSort(b))
      }
    }

    val sorted = mergeSort(A)
    val n = sorted.view.zip(sorted.view.tail).foldLeft(0) { case (acc, (prev, curr)) =>
      if (prev != curr) acc + 1
      else acc
    }

    if (sorted.nonEmpty) n + 1
    else n
  }
}
