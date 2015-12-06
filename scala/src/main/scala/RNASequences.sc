import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable, IndexedSeqLike}
trait Base {
  val value: Int
}
case object A extends Base { val value = 0 }
case object B extends Base{ val value = 1 }
case object C extends Base{ val value = 2 }
case object D extends Base{ val value = 3 }
def toBinary(value: Int, digits: Int = 32) =
  String.format("%" + digits + "s", value.toBinaryString).replace(' ', '0')
final class RNASeq1 private (size: Int)
  extends IndexedSeq[Base]
  with IndexedSeqLike[Base, RNASeq1]{
  import RNASeq1._
  val LengthOfGroup = 16
  val groups = new Array[Int]((size / LengthOfGroup) + 1)
  override def length: Int = size
  override def apply(index: Int): Base =  {
   assert(index < size)
    val groupIndex = index / LengthOfGroup
    val group = groups(groupIndex)
    val offset = 30 - (index % LengthOfGroup) * 2
    val mask = 3 << offset
    fromInt((mask & group) >> offset)
  }
  def set(rna: Base, index: Int): Unit = {
    assert(index < size)
    val groupIndex = index / LengthOfGroup
    val offset = index % LengthOfGroup
    val oldValue = groups(groupIndex)
    groups(groupIndex) = resetBit(oldValue, offset) | toBit(rna, offset)
  }

  override def toString() = {
    (0 to size -1).map { idx => apply(idx) } mkString("")
  }

  override def newBuilder: mutable.Builder[Base, RNASeq1] = {
    new ArrayBuffer[Base] mapResult fromSeq
  }
}
object RNASeq1 {
  val sets = Seq(A, B, C, D).map { base => base.value -> base }.toMap
  def fromInt(n: Int): Base = sets(n)
  def resetBit(n: Int, index: Int) = {
    val offset = 30 - index * 2
    val mask = 3 << offset // ex) 3 << 2 = 0011 to 1100
    ~mask & n
  }

  def toBit(rna: Base, index: Int): Int = {
    val offset = 30 - index * 2
    rna.value << offset
  }

  def fromSeq(rnas: Seq[Base]): RNASeq1 = {
    val rnaSeq = new RNASeq1(rnas.size)
    rnas.zipWithIndex.foreach { case (r, idx) =>
      println(s"$r, $idx")
      rnaSeq.set(r, idx)
    }
    rnaSeq
  }
}
val size = 17
val rnas = (0 to size -1).map { n =>
  val r = n % 3 + 1
  RNASeq1.fromInt(r)
}
val rna = RNASeq1.fromSeq(rnas)
rna(0)
rna(1)
rna(2)
//rna filter { a => A != a }
//rna filter { a => a != B }
//rna filter { a => a != C }
//rna filter { a => a != D }
//val xs = List(A, B, C, D)
//val fromXs = RNASeq1.fromSeq(xs)
//fromXs

