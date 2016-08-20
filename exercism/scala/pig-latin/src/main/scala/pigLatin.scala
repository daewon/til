object PigLatin {
  // **Rule 1**: If a word begins with a vowel sound, add an "ay" sound to
  //   the end of the word.

  // **Rule 2**: If a word begins with a consonant sound, move it to the
  //   end of the word, and then add an "ay" sound to the end of the word.

  val vowel = Set("a", "e", "i", "o", "u")

  def isVowel(in: String): Boolean = vowel.contains(in)

  def isConsonant(in: String): Boolean = !isVowel(in)

  def translate(in: String): String =
    if (isVowel(in.head.toString)) in + "ay"
    else {
      val (others, consonants) = in.split("").span(isVowel)
      val (consonants2, others2) = in.split("").span(isConsonant)

      others2.mkString + consonants2.mkString + "ay"
    }
}
