package p;

public class A {
  public static interface I {
    default int m() { return 1; }
  }

  public static interface J extends I {
    @Override default int m() { return 2; }
  }

  public static class C implements I {
    @Override public final int m() { return 3; }
  }

  public static class D extends C implements J { }
}
