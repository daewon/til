public class HelloWorld {
  public static String hello(String s) {
    if (null == s || s.equals("")) {
      return "Hello, World!";
    } else {
      return "Hello, " + s + "!";
    }
  }
}
