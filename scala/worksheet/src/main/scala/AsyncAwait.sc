// http://engineering.roundupapp.co/the-future-is-not-good-enough-coding-with-async-await/

import scala.concurrent._
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global //"Thread pool" to run futures
import scala.async.Async._ //'async/await' macros blocks and implicits

//Futures, Promises and goodies
val (f1, f2, f3) = (Future(1), Future(2), Future(3))

f1 onSuccess { case r1 =>
  f2 onSuccess { case r2 =>
    f3 onSuccess { case r3 =>
      println(s"Sum:  ${r1 + r2 + r3}")
    }
  }
}

val seqFuture = Future.sequence(Seq(f1, f2, f3))
seqFuture onComplete {
  case Success(nums) => println(nums)
  case Failure(ex) => println("failed")
}

async {
  val (f1, f2, f3) = (Future(1), Future(2), Future(3))

  val s = await(f1) + await(f2) + await(f3)
  println(s"Sum:  $s")
} onFailure { case e => /* Handle failure */ }
