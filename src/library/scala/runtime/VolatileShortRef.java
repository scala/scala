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

public final class VolatileShortRef implements java.io.Serializable {
    private static final long serialVersionUID = 4218441291229072313L;

    volatile public short elem;
    public VolatileShortRef(short elem) { this.elem = elem; }
    public String toString() { return java.lang.Short.toString(elem); }

    public static VolatileShortRef create(short e) { return new VolatileShortRef(e); }
    public static VolatileShortRef zero() { return new VolatileShortRef((short)0); }
}
