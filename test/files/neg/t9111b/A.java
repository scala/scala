public class A {
    public static class P {
      public static class T { public void f() { } }
    }
    public static class T { public void g() { } }
    public static class Inner extends P {
      public class Deeper {
        public void foo(T t) { t.f(); }
      }
    }
  }
  