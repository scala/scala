public class A_1 {
  interface Fun {
    String m(String s);
  }
  public static final String test() {
    Fun f = s -> s.trim();
    return f.m(" eh ");
  }
}
