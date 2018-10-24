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
import java.lang.ref.SoftReference;
import java.lang.reflect.Method;

public final class StructuralCallSite {

    private Class<?>[] parameterTypes;
    private SoftReference<MethodCache> cache = new SoftReference<>(new EmptyMethodCache());

    private StructuralCallSite(MethodType callType) {
        parameterTypes = callType.parameterArray();
    }

    public MethodCache get() {
        MethodCache cache = this.cache.get();
        if (cache == null) {
            cache = new EmptyMethodCache();
            this.cache = new SoftReference<>(cache);
        }
        return cache;
    }

    public Method find(Class<?> receiver) {
        return get().find(receiver);
    }

    public Method add(Class<?> receiver, Method m) {
        cache = new SoftReference<MethodCache>(get().add(receiver, m));
        return m;
    }
    public Class<?>[] parameterTypes() {
        return parameterTypes;
    }

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName,
                                     MethodType invokedType, MethodType reflectiveCallType) throws Throwable {
        StructuralCallSite structuralCallSite = new StructuralCallSite(reflectiveCallType);
        return new ConstantCallSite(MethodHandles.constant(StructuralCallSite.class, structuralCallSite));
    }
}
