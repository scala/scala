//> using jvm 16+
package lib;

/** Contains C.DStatic, only allowed since JDK 16 */
public class RawTypes2 {

  public class C<T> {
    public static class DStatic<U> {} // illegal in Java < 16
  }

  public static void mis_Raw_Raw(C.DStatic d) {} // illegal in Java < 16
  public static void mis_Raw_Gen(C.DStatic<String> d) {} // illegal in Java < 16

}
