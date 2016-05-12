package io.daewon.parser

/**
  * simple query language spec
  *
  * from 1, 2 in A and 3, 4, in B
  * as stepA
  *
  *
  */

object SimpleQuery {

  case class Param(label: String, ids: Int*)

  case class Step(name: String, params: Param*)

  case class Query(params: Step*)

  Query(
    Step("stepA",
      Param("A", 1, 2),
      Param("B", 1, 2)
    )
  )
}
