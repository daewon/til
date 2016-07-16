object WordProblem {

  val opMap: Map[String, (Int, Int) => Int] = Map(
    "+" -> (_ + _),
    "-" -> (_ - _ ),
    "*" -> (_ * _),
    "/" -> (_ / _)
  )

  val IntRegEx = """(-?\d+)""".r

  val pattern = """(-?\d+)|(\+|\-|\*|\/)*""".r

  def normalize(in: String): String = in
    .replace("What is", "")
    .replace("by", "")
    .replace("?", "")
    .replace("plus", "+")
    .replace("minus", "-")
    .replace("multiplied", "*")
    .replace("divided", "/")
    .trim

  def apply(_in: String): Option[Int] = {
    val in = normalize(_in)

    val tokens = pattern.findAllIn(in).matchData.filter { m =>
      m.group(0).nonEmpty
    }.toList.map(_.toString)

    def eval(ls: List[String]): String = ls match {
      case hd :: Nil => hd.toString
      case l :: op :: r :: tl =>
        val fn = opMap(op)
        val ret = fn(l.toInt, r.toInt)

        eval(ret.toString :: tl)
    }

    val ret = eval(tokens)
    Option(ret.toInt)
  }
}
