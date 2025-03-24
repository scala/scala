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

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.invoke.SerializedLambda;

class MethodHandleConstants {
    // static final MethodHandles are optimized by the JIT (https://stackoverflow.com/a/14146641/248998)
    static final MethodHandle LAMBDA_DESERIALIZE_DESERIALIZE_LAMBDA;

    static {
        LAMBDA_DESERIALIZE_DESERIALIZE_LAMBDA = lookupDeserialize();
    }

    private static MethodHandle lookupDeserialize() {
        try {
            return MethodHandles.lookup().findVirtual(Class.forName("scala.runtime.LambdaDeserialize"), "deserializeLambda", MethodType.methodType(Object.class, SerializedLambda.class));
        } catch (NoSuchMethodException | IllegalAccessException | ClassNotFoundException e) {
            throw new ExceptionInInitializerError(e);
        }
    }
}
