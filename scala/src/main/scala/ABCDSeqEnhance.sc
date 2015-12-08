import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.{IndexedSeqLike, mutable}

trait Base

case object A extends Base

case object B extends Base

case object C extends Base

case object D extends Base

class ABCDSeq private(val length: Int, groups: Array[Byte])
  extends IndexedSeq[Base]
  with IndexedSeqLike[Base, ABCDSeq] {

  import ABCDSeq._

  override def apply(n: Int): Base = {
    val groupIndex = n / GroupSize
    val group = groups(groupIndex)
    val index = n % GroupSize // 0, 1, 2, 3
    val shiftOffset = index * 2

    fromInt((group >>> shiftOffset) & Mask)
  }

  override def toString() = {
    (0 until length).map { idx => apply(idx) } mkString ("")
  }

  override def newBuilder: mutable.Builder[Base, ABCDSeq] = newBuilderFrom

  override def foreach[U](f: Base => U): Unit = {
    var index = 0
    var group = 0

    while (index < length) {
      group = if (index % GroupSize == 0) groups(index / GroupSize) else group >> S
      f.apply(fromInt(group & Mask))
      index += 1
    }
  }
}

object ABCDSeq {
  val Mask = 3
  // 0011
  val GroupSize = 4
  // (8 / 2)
  val S = 2 // need two bit for save ABCD seq

  val fromInt: Int => Base = Array(A, B, C, D)
  val toInt: Base => Int = Map(A -> 0, B -> 1, C -> 2, D -> 3)

  def newBuilderFrom: mutable.Builder[Base, ABCDSeq] = new ArrayBuffer[Base] mapResult fromSeq

  implicit val abcdCanBuildFrom: CanBuildFrom[ABCDSeq, Base, ABCDSeq] =
    new CanBuildFrom[ABCDSeq, Base, ABCDSeq] {
      override def apply(from: ABCDSeq): mutable.Builder[Base, ABCDSeq] = newBuilderFrom

      override def apply(): mutable.Builder[Base, ABCDSeq] = newBuilderFrom
    }

  def fromSeq(seqs: Seq[Base]): ABCDSeq = {
    val size = seqs.size
    val groups = new Array[Byte]((size / GroupSize) + 1)

    (0 until size) foreach { n =>
      val groupIndex = n / GroupSize
      val index = n % GroupSize // 0, 1, 2, 3
      val shiftOffset = index * 2 // 0 -> 0, 1 -> 2, 2 -> 4, 3 -> 6
      val base = toInt(seqs(n))
      val currentVal = base << shiftOffset

      val group = groups(groupIndex)
      groups(groupIndex) = (group | currentVal).toByte
    }

    new ABCDSeq(size, groups)
  }
}

val seq = ABCDSeq.fromSeq(Seq(A, B, C, D, A, B, C, D))

// all return type is ABCDSeq cause: CanBuildFrom with newBuilder
seq.head
seq.last
seq.take(3)
seq.drop(3)
seq.filter { base => base == A }
seq.map { base => D }
seq.init
seq.splitAt(3)
seq.partition( base => base == A || base == C)
