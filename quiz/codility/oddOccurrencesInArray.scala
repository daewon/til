import scala.collection.JavaConversions._

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution {
    def solution(A: Array[Int]): Int = {
        // write your code in Scala 2.10
        val set = Set.empty[Int]
        val res = A.foldLeft(set) { case (acc, curr) =>
            if (acc.contains(curr)) acc - curr
            else acc + curr 
        }
        
        res.head
    }
}

