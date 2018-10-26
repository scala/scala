public class A_1 {
  // non-trivial method to avoid it from being inlined
  public static final int bar() { return Integer.valueOf("123") + Integer.valueOf("321"); }
}
