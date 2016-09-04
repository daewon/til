import java.util.*;
import java.util.stream.*;

public class Etl {
  public Map<String, Integer> transform(Map<Integer, List<String>> old) {
    Map<String, Integer> inverted = old
      .entrySet()
      .stream()
      .flatMap(e -> e.getValue().stream().map(k -> new AbstractMap.SimpleEntry<>(k.toLowerCase(), e.getKey())))
      .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

    return inverted;
  }
}
