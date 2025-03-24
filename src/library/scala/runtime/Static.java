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

package scala.runtime;

import java.lang.invoke.*;

public final class Static {
    private Static() {
    }

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName, MethodType invokedType, MethodHandle handle, Object... args) throws Throwable {
        Object value = handle.invokeWithArguments(args);
        return new ConstantCallSite(MethodHandles.constant(invokedType.returnType(), value));
    }
}
