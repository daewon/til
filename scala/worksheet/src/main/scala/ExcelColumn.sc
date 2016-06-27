/**
  * A == 1
  * B == 2
  * ..
  * ..
  * V == 22
  * Z = 26
  * ..
  * ..
  * AA = 27
  * AB = 28
  * ..
  * ..
  * CV == 100
  */

implicit class RichInt(n: Int) {
  val excelMap = (0 to 25).zip('A' to 'Z').toMap

  def toExcelColumn = {
    //    def _toExcel(_n: Int): List[Char] = {
    //      val n = _n - 1
    //      if (n / 26 == 0) List(excelMap(n % 26))
    //      else excelMap(n % 26) :: _toExcel(n / 26)
    //    }
    //
    //    _toExcel(n).reverse.mkString("")
    //
    // start point is n-1, excelColumn dosn't have zero value it's starts from 1(A)
    val stream = Stream.iterate(n - 1) { n => (n / 26) - 1 }.takeWhile(_ > -1)
    stream.reverse.map(n => excelMap(n % 26)).mkString("")
  }
}

implicit class RichString(str: String) {
  val excelMap = ('A' to 'Z').zip(1 to 26).toMap

  def toExcelNum: Int = {
    //    var carry = 1
    //    var result = 0
    //    var all = str.reverse
    //
    //    while (all.length > 0) {
    //      val num = excelMap(all.head)
    //      result += num * carry
    //
    //      carry *= 26
    //      all = all.tail
    //    }
    //    result

    val carries = Stream.iterate(1)(26 *)
    str.reverse.map(excelMap(_)).zip(carries).map { case (n, carry) => n * carry }.sum
  }
}

"A".toExcelNum

26.toExcelColumn

"AA".toExcelNum.toExcelColumn
"CV".toExcelNum.toExcelColumn

1.toExcelColumn == "A".toExcelNum.toExcelColumn
26.toExcelColumn == "Z".toExcelNum.toExcelColumn

27.toExcelColumn == "AA".toExcelNum.toExcelColumn
28.toExcelColumn == "AB".toExcelNum.toExcelColumn

100.toExcelColumn == "CV".toExcelNum.toExcelColumn
1000.toExcelColumn == "ALL".toExcelNum.toExcelColumn
