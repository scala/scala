//> using filter unchecked

package p;

public class A {
  public static interface I {
    public I w();
  }

  public static interface J<R extends J<R>> extends I {
    @Override public R w();
  }

  public static interface K extends I {
    @Override public K w();

    public default String mK() { return "K"; }
  }

  /* package-private */ static class B<R extends J<R>> implements J<R> {
    @Override public R w() { return (R) this; }
  }

  public static class C<R extends J<R>> extends B<R> implements J<R> { }

  // OK in Java, also OK in Scala
  public static class Test extends C<Test> implements K { }
}
