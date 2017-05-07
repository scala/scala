public class Args_0 {
  public static @interface Ann {
    String value();
  }

  public static final String konst = "konst";
  public static final String nokst = "no" + "kst";

  @Ann("a")          public static int x1 = 0; // parsed
  @Ann(Args_0.konst) public static int x3 = 0; // parsed
  @Ann("a" + "b")    public static int x2 = 0; // dropped
  @Ann(Args_0.nokst) public static int x4 = 0; // dropped (error w/ type-checking)
}
