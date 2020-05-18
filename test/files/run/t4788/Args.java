public class Args {
  public static @interface Ann {
    String value();
  }

  public static final String konst = "konst";
  public static final String nokst = "no" + "kst";

  @Ann("a")        public static int x1 = 0; // parsed
  @Ann(Args.konst) public static int x3 = 0; // parsed
  @Ann("a" + "b")  public static int x2 = 0; // dropped
  @Ann(Args.nokst) public static int x4 = 0; // dropped
}
