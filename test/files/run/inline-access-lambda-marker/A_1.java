package p;

interface PkgMarker {}

public class A_1 {
  public static int pub() { return 1; }
  public static Object mkMarked() {
    java.util.function.IntSupplier f = (java.util.function.IntSupplier & PkgMarker) A_1::pub;
    return f;
  }
}
