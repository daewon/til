import fastparse.all._

val ws = P(" ".rep)
lazy val trueVal = P("true").!.map { _ => true }
lazy val falseVal = P("false").!.map { _ => false }
lazy val bool = P(trueVal | falseVal)

lazy val and: P[Boolean] = P(bool ~ (ws ~ "&&" ~ ws ~ bool).rep).map {
  case (b, bs) => bs.foldLeft(b) {
    case (acc, curr) => acc && curr
  }
}

lazy val or: P[Boolean] = P(and ~ (ws ~ "||" ~ ws ~ or).rep).map {
  case (b, bs) => bs.foldLeft(b) {
    case (acc, curr) => acc || curr
  }
}


val parser = P(ws ~ or ~ ws ~ End)

parser.parse("false && true || true")
parser.parse("false || true && false ")
