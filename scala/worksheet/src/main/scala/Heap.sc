import scala.annotation.tailrec
import scala.collection._
import scala.util.Random


val testMaxHeap = {
  val heap = new MinHeap[Int]()
  Random.shuffle(1 to 1000) foreach { n => heap.push(n) }
  (1 to 500) foreach { n =>
    heap.pop
  }
  heap
}
/**
  * Heap data structure from scratch
  */
class MinHeap[A](implicit ord: Numeric[A]) extends Heap[A](ord)

class MaxHeap[A](implicit ord: Numeric[A]) extends Heap[A](ord) {
  override def compare(a: A, b: A): Int = super.compare(a, b) * -1
}

abstract class Heap[A](comparer: Numeric[A]) {
  def compare(a: A, b: A): Int = comparer.compare(a, b)
  import Heap._

  private val buffer = new mutable.ArrayBuffer[A]()

  def push(elem: A): Unit = {
    buffer.append(elem)
    bubbleUp(lastIndex)
  }

  def peek = buffer(0)

  def pop() = {
    if (buffer.nonEmpty) {
      swap(buffer, 0, lastIndex)
      buffer.remove(lastIndex)
      bubbleDown(0)
    }
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

object Heap {
  def swap[T](buffer: mutable.ArrayBuffer[T], a: Int, b: Int) = {
    val tmp = buffer(a)
    buffer(a) = buffer(b)
    buffer(b) = tmp
  }


}
