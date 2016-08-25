case class Acronym(phrase: String) {
  def abbreviate: String = {
    val regEx = """[\s\-]([a-zA-Z])|([A-Z][a-zA-Z])+""".r
    val matched = regEx.findAllMatchIn(phrase)
    val subGroups = matched.flatMap { m =>
      m.subgroups.filterNot(_ == null).map(_.head.toString.toUpperCase)
    }

    subGroups.mkString
  }
}
