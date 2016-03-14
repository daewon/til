import org.scalatest._

class HammingSpecs extends FlatSpec with Matchers {
  it should "detect no difference between empty strands" in {
    Hamming.compute("", "") should be (0)
  }

  it should "detect no difference between identical strands" in {

    Hamming.compute("GGACTGA", "GGACTGA") should be (0)
  }

  it should "detect complete hamming distance in small strand" in {

    Hamming.compute("ACT", "GGA") should be (3)
  }

  it should "give hamming distance in off by one strand" in {

    Hamming.compute("GGACGGATTCTG", "AGGACGGATTCT") should be (9)
  }

  it should "give small hamming distance in middle somewhere" in {

    Hamming.compute("GGACG", "GGTCG") should be (1)
  }

  it should "give a larger distance" in {

    Hamming.compute("ACCAGGG", "ACTATGG") should be (2)
  }

  it should "be undefined for strands of unequal length" in {

    an[IllegalArgumentException] should be thrownBy {
      Hamming.compute("AAACTAGGGG", "AGGCTAGCGGTAGGAC")
    }
  }
}
