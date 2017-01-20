public class A_1 {
  public static class P {
    public static class T { public int f() { return 1; } }
  }
  public static class T { public int g() { return 2; } }
  public static class Inner extends P {
    public class Deeper {
      public int foo(T t) { return t.f(); }
    }
  }

  public static void crossCheck() {
    Inner i = new Inner();
    Inner.Deeper d = i.new Deeper();
    d.foo(new P.T());
  }
}
