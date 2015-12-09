import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.{IndexedSeqLike, TraversableLike, mutable}
trait Base {
  val value: Int
}
case object A extends Base {
  val value = 0
}
case object B extends Base {
  val value = 1
}

case object C extends Base {
  val value = 2
}

case object D extends Base {
  val value = 3
}

object ABCDSeq {
  val sets = Seq(A, B, C, D).map { base => base.value -> base }.toMap
  val Mask = 3 // 0011

  def fromInt(n: Int): Base = sets(n)

  def resetBit(n: Int, index: Int) = {
    val offset = 30 - index * 2
    val mask = Mask << offset // ex) 3 << 2 = 0011 to 1100
    ~mask & n
  }

  def toBit(rna: Base, index: Int): Int = {
    val offset = 30 - index * 2
    rna.value << offset
  }

  def fromSeq(rnas: Seq[Base]): ABCDSeq = {
    val rnaSeq = new ABCDSeq(rnas.size)
    rnas.zipWithIndex.foreach { case (r, idx) =>
      rnaSeq.set(r, idx)
    }
    rnaSeq
  }

  def newBuilder = new ArrayBuffer[Base] mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[ABCDSeq, Base, ABCDSeq] =
    new CanBuildFrom[ABCDSeq, Base, ABCDSeq] {
      def apply(): mutable.Builder[Base, ABCDSeq] = newBuilder

      def apply(from: ABCDSeq): mutable.Builder[Base, ABCDSeq] = newBuilder
    }
}

def toBinary(value: Int, digits: Int = 32) =
  String.format("%" + digits + "s", value.toBinaryString).replace(' ', '0')

class ABCDSeq private(size: Int)
  extends IndexedSeq[Base]
  with IndexedSeqLike[Base, ABCDSeq] {

  import ABCDSeq._

  val LengthOfGroup = 16
  val groups = new Array[Int]((size / LengthOfGroup) + 1)

  override def foreach[U](f: Base => U): Unit = {
    var index = 0
    var group = 0

    while (index < size) {
      val offset = index % LengthOfGroup
      if (offset == 0) group = groups(index / LengthOfGroup)
      val bit = group >>> (30 - offset * 2)
      f(fromInt(bit & Mask))
      index += 1
    }
  }

  override def length: Int = size
  override def apply(index: Int): Base = {
    assert(index < size)
    val groupIndex = index / LengthOfGroup
    val group = groups(groupIndex)
    val offset = 30 - (index % LengthOfGroup) * 2
    fromInt((group >>> offset) & Mask)
  }

  def set(rna: Base, index: Int): Unit = {
    assert(index < size)
    val groupIndex = index / LengthOfGroup
    val offset = index % LengthOfGroup
    val oldValue = groups(groupIndex)
    groups(groupIndex) = resetBit(oldValue, offset) | toBit(rna, offset)
  }

  override def toString() = {
    (0 to size - 1).map { idx => apply(idx) } mkString ("")
  }

  override def newBuilder: mutable.Builder[Base, ABCDSeq] = ABCDSeq.newBuilder
}
val size = 17
val rnas = (0 to size - 1).map { n =>
  val r = n % 3 + 1
  ABCDSeq.fromInt(r)
}
val rna = ABCDSeq.fromSeq(rnas)
rna filter { a => A != a }
rna filter { a => a != B }
rna filter { a => a != C }
rna filter { a => a != D }
val xs = List(A, B, C, D)
val fromXs = ABCDSeq.fromSeq(xs)
fromXs take 3 // return type: ABCDSeq, because extends with IndexedSeqLike
fromXs drop 3 // return type: ABCDSeq, because extends with IndexedSeqLike
fromXs map {
  case A => B
  case _ => C
} // return type: ABCDSeq, because implement CanBuildFrom

