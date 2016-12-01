class Pangrams {
  public static boolean isPangram(String in) {
    in
      .chars()
      .mapToObj(c -> (char)c)
      .forEach(System.out::println);

    return false;
  }
}
