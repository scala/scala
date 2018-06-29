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

import java.io.Serializable;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.util.HashSet;
import java.util.Set;

/** A serialization proxy for singleton objects */
public final class ModuleSerializationProxy implements Serializable {
    private static final long serialVersionUID = 1L;
    private final Class<?> moduleClass;
    private static final ClassValue<Object> instances = new ClassValue<Object>() {
        @Override
        protected Object computeValue(Class<?> type) {
            try {
                return AccessController.doPrivileged((PrivilegedExceptionAction<Object>) () -> type.getField("MODULE$").get(null));
            } catch (PrivilegedActionException e) {
                return rethrowRuntime(e.getCause());
            }
        }
    };

    private static Object rethrowRuntime(Throwable e) {
        Throwable cause = e.getCause();
        if (cause instanceof RuntimeException) throw (RuntimeException) cause;
        else throw new RuntimeException(cause);
    }

    public ModuleSerializationProxy(Class<?> moduleClass) {
        this.moduleClass = moduleClass;
    }

    @SuppressWarnings("unused")
    private Object readResolve() {
        return instances.get(moduleClass);
    }
}
