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

public final class VolatileObjectRef<T> implements java.io.Serializable {
    private static final long serialVersionUID = -9055728157600312291L;

    volatile public T elem;
    public VolatileObjectRef(T elem) { this.elem = elem; }
    @Override
    public String toString() { return String.valueOf(elem); }

    public static <U> VolatileObjectRef<U> create(U e) { return new VolatileObjectRef<U>(e); }
    public static VolatileObjectRef<Object> zero() { return new VolatileObjectRef<Object>(null); }
}
