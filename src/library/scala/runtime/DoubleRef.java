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

public final class DoubleRef implements java.io.Serializable {
    private static final long serialVersionUID = 8304402127373655534L;

    public double elem;
    public DoubleRef(double elem) { this.elem = elem; }
    public String toString() { return java.lang.Double.toString(elem); }

    public static DoubleRef create(double e) { return new DoubleRef(e); }
    public static DoubleRef zero() { return new DoubleRef(0); }
}
