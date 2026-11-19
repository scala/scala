package p;

public class C {
  public static int pub() { return 1; }
  public static Object mk() { PkgFI f = C::pub; return f; }
  public static int run(Object o) { return ((PkgFI) o).run(); }
}
