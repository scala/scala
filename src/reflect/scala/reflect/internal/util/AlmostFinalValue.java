/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.internal.util;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MutableCallSite;

/**
 * Represents a value that is wrapped with JVM machinery to allow the JVM
 * to speculate on its content and effectively optimize it as if it was a constant.
 *
 * Originally from the JSR-292 cookbook created by Rémi Forax:
 * https://code.google.com/archive/p/jsr292-cookbook/.
 *
 * Implemented in Java because using `static`s for the method handles and all the other
 * fields is extremely important for the JVM to correctly optimize the code, and
 * we cannot do that if we make `Statistics` an object extending `MutableCallSite`
 * in Scala.
 *
 * Subsequently specialised for booleans, to avoid needless Boolean boxing.
 *
 * Finally reworked to default to false and only allow for the value to be toggled on,
 * using Rémi Forax's newer "MostlyConstant" as inspiration, in https://github.com/forax/exotic.
 */
final class AlmostFinalValue {
  private static final MethodHandle K_FALSE = MethodHandles.constant(boolean.class, false);
  private static final MethodHandle K_TRUE  = MethodHandles.constant(boolean.class, true);

  private final MutableCallSite callsite = new MutableCallSite(K_FALSE);
          final MethodHandle    invoker  = callsite.dynamicInvoker();

  void toggleOnAndDeoptimize() {
    if (callsite.getTarget() == K_TRUE) return;
    callsite.setTarget(K_TRUE);
    MutableCallSite.syncAll(new MutableCallSite[] { callsite });
  }
}
