import java.util.regex.*;
import java.util.*;
import java.util.stream.*;
import java.util.Map.*;
import java.util.AbstractMap.*;

public class WordCount {
  private ArrayList<String> getWords(String in) {
    Pattern p = Pattern.compile("\\w+");
    Matcher m = p.matcher(in.toLowerCase());

    ArrayList<String> words = new ArrayList<String>();
    while (m.find()) {
      String matched = m.group();
      words.add(matched);
    }

    return words;
  }

  public Map<String, Integer> phrase(String in) {
    Map<String, Integer> wordsCount = getWords(in)
      .stream()
      .map(w -> new SimpleImmutableEntry<>(w, 1))
      .collect(Collectors.toMap(Entry::getKey, Entry::getValue, (a, b) -> a + b));

    return wordsCount;
  }
}
