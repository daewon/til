import scala.collection.JavaConversions._

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution {
    def solution(A: Array[Int]): Int = {
        // write your code in Scala 2.10
        var set = Set.empty[Int]
        
        A.foreach { a =>
            if (set.contains(a)) set = set - a
            else set = set + a
        }
        set.head
    }
}
