object Ocr {
  val rowSz = 4
  val colSz = 3

  val digitMap = Map(
    Seq(
      " _ ",
      "| |",
      "|_|",
      "   ").mkString("\n") -> "0",
    Seq(
      "   ",
      "  |",
      "  |",
      "   ").mkString("\n") -> "1",
    Seq(
      " _ ",
      " _|",
      "|_ ",
      "   ").mkString("\n") -> "2",
    Seq(
      " _ ",
      " _|",
      " _|",
      "   ").mkString("\n") -> "3",
    Seq(
      "   ",
      "|_|",
      "  |",
      "   ").mkString("\n") -> "4",
    Seq(
      " _ ",
      "|_ ",
      " _|",
      "   ").mkString("\n") -> "5",
    Seq(
      " _ ",
      "|_ ",
      "|_|",
      "   ").mkString("\n") -> "6",
    Seq(
      " _ ",
      "  |",
      "  |",
      "   ").mkString("\n") -> "7",
    Seq(
      " _ ",
      "|_|",
      "|_|",
      "   ").mkString("\n") -> "8",
    Seq(
      " _ ",
      "|_|",
      " _|",
      "   ").mkString("\n") -> "9"
  ) withDefault( _ => "?")
}

case class Ocr(in: String) {
  import Ocr._

  def splitColumn(row: Seq[String]): Seq[String] = {
    val len = row.map(_.length).sum

    var splited = row
    var ret = Vector.empty[String]
    var i = 0

    while(i < len) {
      val num = splited.map(_.take(colSz)).mkString("\n")
      splited = splited.map(_.drop(colSz))

      ret = ret :+ num
      i += rowSz * colSz
    }

    ret
  }

  def rows: Iterator[Array[String]] = in.split("\n").grouped(rowSz)

  def convert: String = rows.map { row =>
    splitColumn(row).map(digitMap).mkString("")
  }.mkString(",")
}
