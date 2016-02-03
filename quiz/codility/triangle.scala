import scala.collection.JavaConversions._
import scala.annotation.tailrec
import scala.collection._
import scala.util.Random


// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")
object Solution {

  object Heap {
    def swap[T](buffer: mutable.ArrayBuffer[T], a: Int, b: Int) = {
      val tmp = buffer(a)
      buffer(a) = buffer(b)
      buffer(b) = tmp
    }
  }

  abstract class Heap[A](comparer: Numeric[A]) {
    import Heap._

    private val buffer = new mutable.ArrayBuffer[A]()

    def compare(a: A, b: A): Int = comparer.compare(a, b)

    def push(elem: A): Unit = {
      buffer.append(elem)
      bubbleUp(lastIndex)
    }

    def peek = buffer(0)

    def pop(): A = {
      val ret = buffer(0)
      swap(buffer, 0, lastIndex)
      buffer.remove(lastIndex)
      bubbleDown(0)

      ret
    }

    @tailrec private def bubbleDown(pIndex: Int): Unit = {
      val opt = (hasLeft(pIndex), hasRight(pIndex)) match {
        case (true, true) =>
          compare(buffer(leftIndex(pIndex)), buffer(rightIndex(pIndex))) match {
            case -1 | 0 => Option(leftIndex(pIndex))
            case 1 => Option(rightIndex(pIndex))
          }
        case (true, false) => Option(leftIndex(pIndex))
        case _ => None
      }

      opt match {
        case Some(targetIndex) =>
          if (compare(buffer(pIndex), buffer(targetIndex)) > 0) {
            swap(buffer, pIndex, targetIndex)
            bubbleDown(targetIndex)
          }
        case None => // do nothing
      }
    }

    @tailrec private def bubbleUp(index: Int): Unit = {
      if (index > 0) {
        val pIndex = parentIndex(index)

        compare(buffer(index), buffer(pIndex)) match {
          case -1 => // current, parent
            swap(buffer, pIndex, index)
            bubbleUp(pIndex)
          case _ => // do nothing
        }
      }
    }

    def lastIndex = buffer.length - 1

    def parentIndex(index: Int): Int = index % 2 match {
      case 0 => index / 2 - 1
      case _ => index / 2
    }

    def hasLeft(index: Int): Boolean = leftIndex(index) <= lastIndex

    def hasRight(index: Int): Boolean = rightIndex(index) <= lastIndex

    def leftIndex(index: Int): Int = index * 2 + 1

    def rightIndex(index: Int): Int = index * 2 + 2

    override def toString() = {
      var ret = Vector.empty[String]
      var i = 0
      var step = 1

      while (i < buffer.size) {
        if (i == step - 1) {
          ret = ret :+ "\n"
          step = step * 2
        }

        ret = ret :+ buffer(i).toString
        i += 1
      }
      ret.mkString("\t")
    }
  }

  class MinHeap[A](implicit ord: Numeric[A]) extends Heap[A](ord)

  def solution(A: Array[Int]): Int = {
    // write your code in Scala 2.10
    val heap = new MinHeap[Int]
    A.foreach { n => heap.push(n) }
    A.zipWithIndex.foreach { case (_, i) => A(i) = heap.pop() }

    var i = 0
    var len = A.length
    var found = 0
    while (i < len-2 && found == 0) {
      val (p, q, r) = (A(i), A(i+1), A(i+2))

      if (p + q > r && q + r > p && r + p > q) found = 1
      i += 1
    }

    found
  }
}
