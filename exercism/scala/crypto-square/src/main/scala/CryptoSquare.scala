// PipeOperator from: http://codereview.stackexchange.com/questions/26707/pipeline-operator-in-scala

object PipeOp {
  implicit class PipelineContainer[F](val value: F) extends AnyVal {
    def |>[G] (f: F => G) = f(value)
  }
}

case class CryptoSquare() {
  import PipeOp._

  def normalizePlaintext(in: String): String =
    in.toLowerCase.filter(ch => ch.isDigit || ch.isLetter)

  def squareSize(in: String): Int =
    normalizePlaintext(in).length.toDouble |> Math.sqrt |> Math.ceil |> (_.toInt)

  def plaintextSegments(in: String): Seq[String] = {
    val normalized = normalizePlaintext(in)
    normalized |> squareSize |> (sz => if (sz == 0) Nil else normalized.grouped(sz).toSeq)
  }

  def ciphertext(in: String): String =
    in |> normalizedCiphertext |> (_.filterNot(_ == ' '))

  def normalizedCiphertext(in: String): String =  {
    val segments = plaintextSegments(in)
    val szOpt = segments.headOption.map(_.length)

    val cipher = segments
      .map(_.padTo(szOpt.get, " "))
      .transpose
      .map(_.filterNot(_ == " ").mkString)
      .mkString(" ")

    cipher
  }
}
