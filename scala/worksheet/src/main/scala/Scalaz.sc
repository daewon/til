import scalaz.Functor
import scalaz.Scalaz._

// the simple tree datastructure
sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
// we need a functor instance for our Tree
implicit def TreeFunctor = new Functor[Tree] {
  override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match {
    case Node(value, left, right) =>
      Node(f(value), map(left)(f), map(right)(f))
    case Empty => Empty
  }
}
// our sample data
val sampleTree: Tree[String] =
  Node("Foo", Node("Bar", Empty, Node("BlubBlub", Empty, Empty)), Empty)

println(sampleTree)
println(sampleTree map (_.length))
println(sampleTree map (_.toLowerCase()))

