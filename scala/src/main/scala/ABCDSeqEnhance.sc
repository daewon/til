trait Base
case object A extends Base
case object B extends Base

case object C extends Base

case object D extends Base

class ABCDSeq private(val length: Int, groups: Array[Byte]) extends IndexedSeq[Base] {
  override def apply(idx: Int): Base = {
    A
  }
}

object ABCDSeq {
  val Mask = 3
  val GroupSize = 4
  val S = 2

  val fromInt: Int => Base = Array(A, B, C, D)
  val toInt: Base => Int = Map(A -> 0, B -> 1, C -> 2, D -> 3)

  def fromSeq(seqs: Seq[Base]): ABCDSeq = {
    val size = seqs.size
    val groups = new Array[Byte](size)

    (0 until size) foreach { n =>
      val groupIndex = (n / GroupSize)
      val group = groups(groupIndex)
      val index = (n % GroupSize) // 0, 1, 2, 3
      val shiftOffset = (8 - 2) - (index * 2)
      val base = toInt(seqs(n))
      val currentVal = base << shiftOffset
      groups(groupIndex) = currentVal.toByte
    }

    new ABCDSeq(size, groups)
  }
}