package scala.reflect.internal.util;

import java.lang.invoke.MethodHandle;

final class AlmostFinalValueBenchmarkStatics {
  static final boolean STATIC_FINAL_FALSE = false;

  private static final AlmostFinalValue ALMOST_FINAL_FALSE    = new AlmostFinalValue();
  private static final MethodHandle ALMOST_FINAL_FALSE_GETTER = ALMOST_FINAL_FALSE.invoker;

  static boolean isTrue() throws Throwable { return (boolean) ALMOST_FINAL_FALSE_GETTER.invokeExact(); }
}
