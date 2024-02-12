package lib;

public class TpeAnnotated {

  private static <T> T unimplemented() {
    return null;
  }

  public static @TpeAnnot int[] f() { return unimplemented(); } // annotates the primitive type int

  // public static int @TpeAnnot [] g() { return unimplemented(); } // errors in nsc/dotc java parser, but is JLS compliant
  // public static int @TpeAnnot [][] h() { return unimplemented(); } // errors in nsc/dotc java parser, but is JLS compliant
  // public static int[] @TpeAnnot [] i() { return unimplemented(); } // errors in nsc/dotc java parser, but is JLS compliant


  public class C {
    public class D {}
  }

  public static <@TpeAnnot T> T foo1() { return unimplemented(); } // annotates the type parameter T
  // public static C.@TpeAnnot D foo2() { return unimplemented(); } // errors in nsc/dotc java parser, but is JLS compliant
  public static @TpeAnnot C.D foo3() { return unimplemented(); } // annotates the type C

}
