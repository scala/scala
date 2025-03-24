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

public final class SymbolLiteral {
    private SymbolLiteral() {
    }

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName,
                                     MethodType invokedType,
                                     String value) throws Throwable {
        ClassLoader classLoader = lookup.lookupClass().getClassLoader();
        MethodType type = MethodType.fromMethodDescriptorString("(Ljava/lang/String;)Lscala/Symbol;", classLoader);
        Class<?> symbolClass = Class.forName("scala.Symbol", false, classLoader);
        MethodHandle factoryMethod = lookup.findStatic(symbolClass, "apply", type);
        Object symbolValue = factoryMethod.invokeWithArguments(value);
        return new ConstantCallSite(MethodHandles.constant(symbolClass, symbolValue));
    }
}
