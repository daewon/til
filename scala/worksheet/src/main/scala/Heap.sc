import scala.collection._
import scala.util.Random

//
val heap = {
  val heap = new Heap[Int]()
  Random.shuffle(1 to 1000) foreach { n => heap.add(n) }
  (1 to 500) foreach { n =>
    heap.pop
  }
  heap
}
class Heap[A] {
  private val buffer = new mutable.ArrayBuffer[A]()
  def add(elem: A)(implicit ec: Numeric[A]): Unit = {
    buffer.append(elem)
    bubbleUp(lastIndex)
  }

  def peek = buffer(0)

  def pop(implicit ec: Numeric[A]) = {
    if (buffer.nonEmpty) {
      swap(0, lastIndex)
      buffer.remove(lastIndex)
      bubbleDown(0)
    }
  }

  def swap(a: Int, b: Int) = {
    val tmp = buffer(a)
    buffer(a) = buffer(b)
    buffer(b) = tmp
  }

  private def bubbleDown(index: Int)(implicit ec: Numeric[A]): Unit = {
    val (hasLeft, hasRight) = (isValidLeft(index), isValidRight(index))
    (hasLeft, hasRight) match {
      case (true, true) =>
        ec.compare(buffer(leftIndex(index)), buffer(rightIndex(index))) match {
          case -1 | 0 =>
            if (ec.compare(buffer(index), buffer(leftIndex(index))) > 0) {
              swap(index, leftIndex(index))
              bubbleDown(leftIndex(index))
            }
          case _ =>
            if (ec.compare(buffer(index), buffer(rightIndex(index))) > 0) {
              swap(index, rightIndex(index))
              bubbleDown(rightIndex(index))
            }
        }

      case (true, false) =>
        if (ec.compare(buffer(index), buffer(leftIndex(index))) > 0) {
          swap(index, leftIndex(index))
          bubbleDown(leftIndex(index))
        }
      case (false, true) =>
        if (ec.compare(buffer(index), buffer(rightIndex(index))) > 0) {
          swap(index, rightIndex(index))
          bubbleDown(rightIndex(index))
        }
      case (false, false) => // do nothing
    }
  }

  private def bubbleUp(index: Int)(implicit ec: Numeric[A]): Unit = {
    if (buffer.length > 1 && index > 0) {
      val pIndex = parentIndex(index)

      val current = buffer(index)
      val parent = buffer(pIndex)

      ec.compare(current, parent) match {
        case 0 | -1 =>
          swap(pIndex, index)
          bubbleUp(pIndex)
        case _ =>
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
}

