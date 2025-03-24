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

package scala.tools.partest.nest;

import java.lang.reflect.Field;

@SuppressWarnings("sunapi")  // also requires passing -XDenableSunApiLintControl to javac
public class UnsafeAccess {
    public final static sun.misc.Unsafe U;

    static {
        U = lookupUnsafe();
    }

    private static sun.misc.Unsafe lookupUnsafe() {
        try {
            sun.misc.Unsafe found = null;
            for (Field field : sun.misc.Unsafe.class.getDeclaredFields()) {
                if (field.getType() == sun.misc.Unsafe.class) {
                    field.setAccessible(true);
                    found = (sun.misc.Unsafe) field.get(null);
                    break;
                }
            }
            if (found == null) throw new IllegalStateException("Can't find instance of sun.misc.Unsafe");
            else return found;
        } catch (Throwable t) {
            throw new ExceptionInInitializerError(t);
        }
    }
}

