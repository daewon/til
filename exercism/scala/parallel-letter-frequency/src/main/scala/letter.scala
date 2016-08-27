import scala.concurrent._
import scala.concurrent.duration._

trait Monoid[M] {
  def zero: M
  def op(a: M, b: M) : M
}

object Frequency {
  implicit val ec = ExecutionContext.global

  type WordCount = Map[Char, Int]

  def mapJoinM[A] = new Monoid[Map[A, Int]] {
    override def zero = Map.empty[A, Int]

    override def op(a: Map[A, Int], b: Map[A, Int]): Map[A, Int] =
      a.foldLeft(b) { case (acc, (k, cnt)) => acc + (k -> (b.getOrElse(k, 0) + cnt)) }
  }

  def frequency(numOfWorker: Int, ls: Seq[String]): WordCount = {
    val words: Seq[Char] = ls.view.flatMap(_.toLowerCase.toCharArray).filter { ch =>
      ch.isValidChar && !ch.isSpaceChar && ch >= '0' && '9' <= ch
    }

    val jobs: Iterator[Seq[Char]] = words.grouped(numOfWorker)

    val jobFutures: Iterator[Future[WordCount]] = jobs.map { charLs =>
      Future(charLs.map(c => Map(c -> 1)).fold(mapJoinM[Char].zero)(mapJoinM[Char].op))
    }

    val reduced: Future[WordCount] =
      Future.fold(jobFutures)(mapJoinM[Char].zero)(mapJoinM[Char].op)

    Await.result(reduced, 10 second)
  }
}
