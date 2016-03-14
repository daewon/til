object Hamming {
  def compute(a: String, b: String): Int = {
    if (a.length != b.length) throw new IllegalArgumentException

    a zip b count {
      case(a, b) => a != b
    }
  }
}
