package p;

import java.lang.invoke.*;

public class C {
  static Pkg make() { return new Pkg(); }

  public static MethodHandle handle() throws Throwable {
    return MethodHandles.lookup().findStatic(C.class, "make", MethodType.methodType(Pkg.class));
  }

  public static int call(MethodHandle h) throws Throwable {
    Pkg x = (Pkg) h.invokeExact();
    return 1;
  }
}
