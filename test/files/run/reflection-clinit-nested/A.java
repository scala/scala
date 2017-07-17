package p1;

public class A {
  static { throww(); }
  static void throww() { throw null; }
  public class Inner { }
  public static class StaticInner { static { throww(); } }
}
