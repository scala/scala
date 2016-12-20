public class A_1 {
  static class C {
    class T {}
  }
  static class D extends C {
    static class T {}
  }
  static class E extends D {

    // An unqualified T resolves to the static class D.T.
    // The scala compiler puts D.T into the companion object of D.
    // When performing a lookup of T, we need to make sure to consider the companion object of D
    // before checking the superclass C, which has an instance member T (SI-9111).

    public T foo() {
      return new D.T(); // cross-check: T is D.T
    }
    public C.T bar() {
      // C.T is not a static class, `this` is implicitly used as enclosing instance - `this.new T()` doesn't work.
      return new C.T();
    }
  }

  // A qualified E.T also resolves to D.T
  public static E.T miz() {
    return new D.T(); // cross-check: E.T is D.T
  }

  public static D.T paz() {
    return new E.T();
  }

  public static C.T tux() {
    C c = new C();
    return c.new T();
  }
}
