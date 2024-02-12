package lib;

public class SomeAnnotated {

  private static <T> T unimplemented() {
    return null;
  }

  @SomeAnnotation("hello")
  public static int method() { return 23; }

  @SomeAnnotation(value = "hello", year = 1996)
  public static int method2() { return 23; }

  @SomeAnnotation(value = "hello", year = 1996, classes = {long.class})
  public static int method3() { return 23; }

}
