public class B_2 {
  private static int res = 0;

  public static void m(char a[]) { res += 10; }
  public static void m(String a) { res += 100; }
  public static void m(Object a) { res += 1000; }

  public static <T> T foo(int a, T... b) { return b[0]; }

  public static <T> T bar(T b[]) { return b[0]; }

  public static void main(String[] args) {
    m(foo(15, "a", "b", "c"));
    if (res != 100)
      throw new Error("bad: "+ res);

    A a = new A();
    m(a.foo(16, "a", "b", "c"));
    if (res != 200)
      throw new Error("bad: " + res);
  }
}
