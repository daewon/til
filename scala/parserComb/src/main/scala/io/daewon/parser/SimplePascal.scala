package io.daewon.parser

import scala.util.parsing.combinator.syntactical.StandardTokenParsers


object SimplePascal extends StandardTokenParsers {
  //  lexical.delimiters ++= Set(";")

  def program = programHeading ~ identifier ~ "."

  def programHeading = "program" ~ identifier ~ ";"

  def block = declarationPart ~ statementPart

  def declarationPart = opt(variableDeclarationPart)

  def variableDeclarationPart = "var" ~ rep(variableDeclaration ~ ";")

  def variableDeclaration = identifier ~ ":" ~ typeAnnotation

  def statementPart = "begin" ~ statementSequence ~ "end"

  def statementSequence = assignment ~ ";"

  def assignment = identifier ~ ":=" ~ (number | boolean)

  def identifier = rep1(letter)

  def letter = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"

  def typeAnnotation = "integer" | "boolean"

  def number = rep1(digit)

  def digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

  def boolean = "true" | "false"
}

object SimplePascalParser {
  val program =
    """
   program main;

   var int: integer;
   var bool: boolean;

   begin
     int = 10;
     bool = false;
   end.
    """
}
