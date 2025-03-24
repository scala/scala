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

public final class VolatileLongRef implements java.io.Serializable {
    private static final long serialVersionUID = -3567869820105829499L;

    volatile public long elem;
    public VolatileLongRef(long elem) { this.elem = elem; }
    public String toString() { return java.lang.Long.toString(elem); }

    public static VolatileLongRef create(long e) { return new VolatileLongRef(e); }
    public static VolatileLongRef zero() { return new VolatileLongRef(0); }
}
