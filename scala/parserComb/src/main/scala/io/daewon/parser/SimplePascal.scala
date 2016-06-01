package io.daewon.parser

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/**
  * program main;
  *
  * var int: integer;
  * var bool: boolean;
  *
  * begin
  *   int = 10;
  *   bool = false;
  * end
  *
  *.
  */
object SimplePascal extends StandardTokenParsers {
  lexical.delimiters ++= Set(";")
  def program = programHeading ~ block ~ "."

  def programHeading = "program" ~ identifier ~ ";"

  def identifier = rep1("rep")

  def block = ""
}

class SimplePascal {
}
