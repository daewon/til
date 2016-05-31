package io.daewon.parser

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object SimplePascal extends StandardTokenParsers {
  lexical.delimiters ++= Set(";")
  def program = programHeading ~ block ~ "."

  def programHeading = "program" ~ identifier ~ ";"

  def identifier = rep1("rep")
}

class SimplePascal {
}
