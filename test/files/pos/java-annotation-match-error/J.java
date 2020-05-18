public class J {
  @Ann(string = "foo" + "bar")
  public static void _1() {}

  @Ann(strings = {"trailing", "commas", "are", "okay", })
  public static void _2() {}

  @Ann(strings = {"lit", "a" + "b"})
  public static void _3() {}

  @Ann(classes = {java.lang.Integer.class, J.class})
  public static void _4() {}

  @Ann(integer = 1 + -(1 + 1))
  public static void _5() {}

  @Ann(integers = {1, 1 + -(1 + 1)})
  public static void _6() {}

  @Ann(integers = {})
  public static void _7() {}
}
