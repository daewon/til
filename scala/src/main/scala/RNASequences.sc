import scala.collection.mutable.ArrayBuffer

trait Base {
  val value: Int
  def toBinary(digits: Int = 32) =
    String.format("%" + digits + "s", value.toBinaryString).replace(' ', '0')
}
case object A extends Base { val value = 0 }
case object B extends Base{ val value = 1 }
case object C extends Base{ val value = 2 }
case object D extends Base{ val value = 3 }


class RNASeq(size: Int) {
  import RNASeq._
  val LengthOfGroup = 16
  val values = new Array[Int]((size / LengthOfGroup))

  def set(rna: Base, offset: Int): Unit = {
    val valueOffstet = offset / LengthOfGroup
    val value = values(valueOffstet)
    values(valueOffstet) = rna.value
  }

  override def toString() = values.length.toString
}

val rna = new RNASeq(16)
rna.set(C, 0)
rna.set(D, 1)
rna.toString()

object RNASeq {
  val sets = Seq(A, B, C, D).map { base => base.value -> base }.toMap
  def fromInt(n: Int): Base = sets(n)

  def toBit(rna: Base, index: Int): Int = {
    val offset = (2 * (16 - (index + 1)))
    val mask = -1 >> offset
    mask & (rna.value << offset)
  }

//  def fromInts(ints: Seq): RNASeq = {
//    val rnaSeq = new RNASeq
//    ints.zipWithIndex.foreach { (n, idx) =>
//      rnaSeq.set(n, idx)
//    }
//    rnaSeq
//  }
}


