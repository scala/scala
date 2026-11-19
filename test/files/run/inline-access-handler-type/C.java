package p;

public class C {
  public static void thrower() { throw new PkgEx(); }
  public static int tryIt(java.util.function.IntSupplier s) {
    try { return s.getAsInt(); } catch (PkgEx e) { return -1; }
  }
}
