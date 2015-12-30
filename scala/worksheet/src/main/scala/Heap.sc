import scala.collection._
import scala.util.Random

val testHeap = {
  val heap = new Heap[Int]()
  Random.shuffle(1 to 1000) foreach { n => heap.push(n) }
  (1 to 500) foreach { n =>
    heap.pop
  }
  heap
}
/**
  * Heap data structure from scratch
  */
class Heap[A] {
  import Heap._
  private val buffer = new mutable.ArrayBuffer[A]()
  def push(elem: A)(implicit ec: Numeric[A]): Unit = {
    buffer.append(elem)
    bubbleUp(lastIndex)
  }

  def peek = buffer(0)

  def pop(implicit ec: Numeric[A]): A = {
    if (buffer.nonEmpty) {
      swap(buffer, 0, lastIndex)
      buffer.remove(lastIndex)
      bubbleDown(0)
    }

    peek
  }

  private def bubbleDown(pIndex: Int)(implicit ec: Numeric[A]): Unit = {
    val (hasLeft, hasRight) = (isValidLeft(pIndex), isValidRight(pIndex))

    val opt = (hasLeft, hasRight) match {
      case (true, true) =>
        ec.compare(buffer(leftIndex(pIndex)), buffer(rightIndex(pIndex))) match {
          case -1 => Option(leftIndex(pIndex))
          case 1 => Option(rightIndex(pIndex))
          case _ => None
        }
      case (true, false) => Option(leftIndex(pIndex))
      case _ => None
    }

    opt.foreach { targetIndex =>
      if (ec.compare(buffer(pIndex), buffer(targetIndex)) > 0) {
        swap(buffer, pIndex, targetIndex)
        bubbleDown(targetIndex)
      }
    }
  }

  private def bubbleUp(index: Int)(implicit ec: Numeric[A]): Unit = {
    if (buffer.length > 1 && index > 0) {
      val pIndex = parentIndex(index)

      val current = buffer(index)
      val parent = buffer(pIndex)

      ec.compare(current, parent) match {
        case -1 =>
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

  def isValidLeft(index: Int): Boolean = leftIndex(index) <= lastIndex

  def isValidRight(index: Int): Boolean = rightIndex(index) <= lastIndex

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
