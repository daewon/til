import java.util.stream.*;
import java.util.Map;
import java.util.AbstractMap.*;

public class DNA {
  private String validDNA = "ACGT";
  private String dna;

  public DNA(String dna) {
    this.dna = dna;
  }

  private Stream<SimpleEntry<Character, Integer>> stringToStream(String dna, int defaultValue) {
    return dna
      .chars()
      .mapToObj(c -> new SimpleEntry<>((char)c, defaultValue));
  }

  public long count(char ch) {
    if (this.validDNA.indexOf(ch) < 0) {
      throw new IllegalArgumentException();
    }

    return this.dna.chars().filter(c -> c == ch).count();
  }

  public Map<Character, Integer> nucleotideCounts() {
    return
      Stream.concat(stringToStream(validDNA, 0), stringToStream(this.dna, 1))
      .collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue(), (a, b) -> a + b));
  }
}
