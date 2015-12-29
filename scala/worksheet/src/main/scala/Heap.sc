import scala.collection._

//
val heap = {
  val heap = new Heap[Int]()
  (0 to 1).reverse foreach { n => heap.add(n) }
  heap
}
class Heap[A] {
  private val buffer = new mutable.ArrayBuffer[A]()
  def add(elem: A)(implicit ec: Numeric[A]): Unit = {
    buffer.append(elem)

    if (buffer.length > 1 ) {
      val last = buffer(lastIndex)
      val parent = buffer(parentIndex(lastIndex))
      ec.compare(last, parent) match {
        case -1 | 0 => println(last)
        case _ =>
      }
    }
  }

  def lastIndex = buffer.length - 1

  def parentIndex(index: Int): Int = index % 2 match {
    case 0 => index / 2 - 1
    case _ => index / 2
  }

  def leftIndex(index: Int): Int = index * 2 + 1
  def rightIndex(index: Int): Int = index * 2 + 2

  override def toString() = {
    var ret = Vector.empty[String]
    var i = 0
    var step = 1

    while(i < buffer.size) {
      if (i == step-1) {
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

