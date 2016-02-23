package io.daewon.fastParser

import fastparse.all._

object Main extends App {
  val ab = P("a" ~ "b")

  val Parsed.Success(_, 2) = ab.parse("ab")

  val Parsed.Failure(parser, 1, _) = ab.parse("aa")

  assert(parser == ("b": P0))
}
