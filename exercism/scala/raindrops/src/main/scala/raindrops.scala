case class Raindrops() {
  def convert(n: Int): String = {
    var out = ""

    if ((n % 3) == 0) out += "Pling"
    if ((n % 5) == 0) out += "Plang"
    if ((n % 7) == 0) out += "Plong"

    if (out == "") n.toString else out
  }
}
