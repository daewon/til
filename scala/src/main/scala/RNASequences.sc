trait Base
case object A extends Base // 00
case object B extends Base // 01
case object C extends Base // 10
case object D extends Base // 11
def toBinary(i: Int, digits: Int = 8) =
  String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')
(3 to 3) map { n =>
  (0 to 30 by 2) foreach { j =>
    val a = toBinary(n << j, 32)
    println(a)
  }
}

