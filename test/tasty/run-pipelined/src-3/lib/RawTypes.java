package lib;

public class RawTypes {

  public class C<T> {
    public class D<U> {
      public class E<V> {}
    }
    // public static class DStatic<U> {} // illegal in Java < 16
  }

  public static class CStatic<T> {
    public class D<U> {}
    public static class DStatic<U> {}
  }

  public static void miii_Raw_Raw_Raw(C.D.E e) {}

  public static void mii_Raw_Raw(C.D d) {}
  // public static void mis_Raw_Raw(C.DStatic d) {} // illegal in Java < 16
  public static void msi_Raw_Raw(CStatic.D d) {}
  public static void mss_Raw_Raw(CStatic.DStatic d) {}

  // public static void mii_Raw_Gen(C.D<String> d) {} // illegal
  // public static void mis_Raw_Gen(C.DStatic<String> d) {} // illegal in Java < 16
  // public static void msi_Raw_Gen(CStatic.D<String> d) {} // illegal
  public static void mss_Raw_Gen(CStatic.DStatic<String> d) {}

  // public static void mii_Gen_Raw(C<String>.D d) {} // illegal
  // public static void mis_Gen_Raw(C<String>.DStatic d) {} // illegal
  // public static void msi_Gen_Raw(CStatic<String>.D d) {} // illegal
  // public static void mss_Gen_Raw(CStatic<String>.DStatic d) {} // illegal

  public static void mii_Gen_Gen(C<String>.D<String> d) {}
  // public static void mis_Gen_Gen(C<String>.DStatic<String> d) {} // illegal
  public static void msi_Gen_Gen(CStatic<String>.D<String> d) {}
  // public static void mss_Gen_Gen(CStatic<String>.DStatic<String> d) {} // illegal
}
