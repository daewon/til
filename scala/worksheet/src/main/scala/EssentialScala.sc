// http://downloads.typesafe.com/website/presentations/ScalaDaysSF2015/essential-scala.pdf

/**
1. Expressions, types, & values
2. Objects and classes
3. Algebraic data types
4. Structural recursion
5. Sequencing computation
6. Type classes
  */

// Sum type: Or: C is A Or B
trait C
trait A extends C
trait B extends C

// Product type: AA has B And C
case class AA(a: B, b: C)


// fold is generic transformer for algebraic data type
val a = Option(10)
a.fold("not evaluated") { n =>
  (n * 100).toString
}






