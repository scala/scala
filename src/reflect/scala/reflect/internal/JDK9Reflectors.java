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

package scala.reflect.internal;

import java.io.IOException;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.jar.JarFile;

public final class JDK9Reflectors {
    private static final MethodHandle RUNTIME_VERSION_PARSE;
    private static final MethodHandle RUNTIME_VERSION;
    private static final MethodHandle RUNTIME_VERSION_MAJOR;
    private static final MethodHandle NEW_JAR_FILE;

    static {
        RUNTIME_VERSION_PARSE = lookupRuntimeVersionParse();
        RUNTIME_VERSION = lookupRuntimeVersion();
        RUNTIME_VERSION_MAJOR = lookupRuntimeVersionMajor();
        NEW_JAR_FILE = lookupNewJarFile();
    }

    public static /*java.lang.Runtime.Version*/ Object runtimeVersionParse(String string) {
        try {
            return RUNTIME_VERSION_PARSE == null ? null : RUNTIME_VERSION_PARSE.invoke(string);
        } catch (Throwable t) {
            return null;
        }
    }

    public static /*java.lang.Runtime.Version*/ Object runtimeVersion() {
        try {
            return RUNTIME_VERSION == null ? null : RUNTIME_VERSION.invoke();
        } catch (Throwable t) {
            return null;
        }
    }

    public static /*java.lang.Runtime.Version*/ Integer runtimeVersionMajor(/*java.lang.Runtime.Version*/ Object version) {
        try {
            return RUNTIME_VERSION_MAJOR == null ? null : (Integer) (int) RUNTIME_VERSION_MAJOR.invoke(version);
        } catch (Throwable t) {
            return null;
        }
    }

    public static JarFile newJarFile(java.io.File file, boolean verify, int mode, /*java.lang.Runtime.Version*/ Object version) throws IOException {
        try {
            if (version == null) return new JarFile(file, verify, mode);
            else {
                return NEW_JAR_FILE == null ? null : (JarFile) NEW_JAR_FILE.invoke(file, verify, mode, version);
            }
        } catch (IOException | IllegalArgumentException | SecurityException ex) {
            throw ex;
        } catch (Throwable t) {
            throw new RuntimeException(t);
        }

    }

    private static MethodHandle lookupRuntimeVersionParse() {
        try {
            return MethodHandles.lookup().findStatic(runtimeVersionClass(), "parse", MethodType.methodType(runtimeVersionClass(), String.class));
        } catch (Throwable t) {
            return null;
        }
    }
    private static MethodHandle lookupRuntimeVersion() {
        try {
            return MethodHandles.lookup().findStatic(java.lang.Runtime.class, "version", MethodType.methodType(runtimeVersionClass()));
        } catch (Throwable t) {
            return null;
        }
    }
    private static MethodHandle lookupRuntimeVersionMajor() {
        try {
            return MethodHandles.lookup().findVirtual(runtimeVersionClass(), "major", MethodType.methodType(Integer.TYPE));
        } catch (Throwable t) {
            return null;
        }
    }
    private static MethodHandle lookupNewJarFile() {
        try {
            return MethodHandles.lookup().findConstructor(java.util.jar.JarFile.class, MethodType.methodType(void.class, java.io.File.class, java.lang.Boolean.TYPE, Integer.TYPE, runtimeVersionClass()));
        } catch (Throwable t) {
            return null;
        }
    }
    private static Class<?> runtimeVersionClass() throws ClassNotFoundException {
        return Class.forName("java.lang.Runtime$Version");
    }
}
