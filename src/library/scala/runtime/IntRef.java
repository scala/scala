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

public final class IntRef implements java.io.Serializable {
    private static final long serialVersionUID = 1488197132022872888L;

    public int elem;
    public IntRef(int elem) { this.elem = elem; }
    public String toString() { return java.lang.Integer.toString(elem); }

    public static IntRef create(int e) { return new IntRef(e); }
    public static IntRef zero() { return new IntRef(0); }
}
