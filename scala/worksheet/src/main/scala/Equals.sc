// http://stackoverflow.com/questions/7681161/whats-the-difference-between-and-equals-in-scala

// == method is juse route to method equals

case class A(n: Int)
A(1) == A(1) // true

case class B(n: Int) {
  override def equals(other: Any) = false
}
B(1) == B(1) // false