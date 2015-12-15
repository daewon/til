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

implicit class richInt(n: Int) {
  val excelMap = (0 to 25).zip('A' to 'Z').toMap

  def _toExcel(_n: Int): List[Char] = {
    val n = _n - 1
    if (n / 26 == 0) List(excelMap(n % 26))
    else excelMap(n % 26) :: _toExcel(n / 26)
  }

  def toExcelColumn = _toExcel(n).reverse.mkString("")
}

implicit class richString(str: String) {
  val excelMap = ('A' to 'Z').zip(1 to 26).toMap

  def toExcelColumn: Int = {
    var carry = 1
    var result = 0
    var all = str.reverse

    while (all.length > 0) {
      val num = excelMap(all.head)
      result += num * carry

      carry *= 26
      all = all.tail
    }

    result
  }
}

1.toExcelColumn == "A".toExcelColumn.toExcelColumn
26.toExcelColumn == "Z".toExcelColumn.toExcelColumn

27.toExcelColumn == "AA".toExcelColumn.toExcelColumn
28.toExcelColumn == "AB".toExcelColumn.toExcelColumn

100.toExcelColumn == "CV".toExcelColumn.toExcelColumn
1000.toExcelColumn == "ALL".toExcelColumn.toExcelColumn
