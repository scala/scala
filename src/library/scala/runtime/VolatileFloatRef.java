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

public final class VolatileFloatRef implements java.io.Serializable {
    private static final long serialVersionUID = -5793980990371366933L;

    volatile public float elem;
    public VolatileFloatRef(float elem) { this.elem = elem; }
    public String toString() { return java.lang.Float.toString(elem); }

    public static VolatileFloatRef create(float e) { return new VolatileFloatRef(e); }
    public static VolatileFloatRef zero() { return new VolatileFloatRef(0); }
}
