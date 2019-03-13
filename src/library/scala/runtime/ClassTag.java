/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.runtime;

import java.lang.invoke.*;

public final class ClassTag {
    private ClassTag() {
    }

    private static final MethodHandle CLASSTAG_APPLY;
    private static final Class<?> CLASSTAG_CLASS;

    static {
        CLASSTAG_CLASS = lookupClass();
        CLASSTAG_APPLY = lookupHandle();
    }

    private static Class<?> lookupClass() {
        try {
            return Class.forName("scala.reflect.ClassTag");
        } catch (ClassNotFoundException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private static MethodHandle lookupHandle() {
        try {
            return MethodHandles.lookup().findStatic(CLASSTAG_CLASS, "apply", MethodType.methodType(CLASSTAG_CLASS, Class.class));
        } catch (NoSuchMethodException | IllegalAccessException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName, MethodType invokedType, Class<?> value) throws Throwable {
        Object tagValue = CLASSTAG_APPLY.invokeWithArguments(value);
        return new ConstantCallSite(MethodHandles.constant(CLASSTAG_CLASS, tagValue));
    }
}
