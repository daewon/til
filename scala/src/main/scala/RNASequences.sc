trait Base {
  val value: Int
}
case object A extends Base { val value = 0 }
case object B extends Base{ val value = 1 }
case object C extends Base{ val value = 2 }
case object D extends Base{ val value = 3 }
def toBinary(value: Int, digits: Int = 32) =
  String.format("%" + digits + "s", value.toBinaryString).replace(' ', '0')
class RNASeq1(size: Int) extends IndexedSeq[Base] {
  import RNASeq1._

  val LengthOfGroup = 16
  val groups = new Array[Int]((size / LengthOfGroup) + 1)

  override def length: Int = size

  override def apply(index: Int): Base =  {
   assert(index < size)

    val groupIndex = index / LengthOfGroup
    val group = groups(groupIndex)

    val offset = 30 - index * 2
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
    (0 until size).map { idx => apply(idx) } mkString("")
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

//  def fromInts(ints: Seq): RNASeq1 = {
//    val rnaSeq = new RNASeq1
//    ints.zipWithIndex.foreach { (n, idx) =>
//      rnaSeq.set(n, idx)
//    }
//    rnaSeq
//  }
}
val rna = new RNASeq1(32)
(0 to 31) foreach { n =>
  val r = n % 4
  rna.set(RNASeq1.fromInt(r), n)
}
rna(0)
rna(1)
rna(2)
rna(3)
rna(4)
rna.toString()
rna.take(3) // It returns vector
