import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

def toFuture[T](a: T): Future[T] = Future(a)

def traverse[A, B, C[A] <: Iterable[A]]
(collection: C[A])(fn: A ⇒ Future[B])(implicit ec: ExecutionContext, cbf: CanBuildFrom[C[B], B, C[B]]): Future[C[B]] = {
  val builder = cbf()
  builder.sizeHint(collection.size)

  collection.foldLeft(Future(builder)) {
    (previousFuture, next) ⇒
      for {
        previousResults ← previousFuture
        next ← fn(next)
      } yield previousResults += next
  } map { builder ⇒  builder.result }
}

traverse(List(1, 2, 3))(toFuture) onComplete { case Success(n) => println(n) }

def traverse[T](ls: Seq[T])(f: T => Future[T]): Future[List[T]] = {
  val initial  = Future.successful(List.empty[T])

  ls.map(f).foldLeft(initial) { case (acc, current) =>
    acc.flatMap { l =>
      current map { n =>
        n +: l
      }
    }
  }
}

traverse(List(1, 2, 3))(toFuture) onComplete { case Success(n) => println(n) }
