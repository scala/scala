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

public final class VolatileByteRef implements java.io.Serializable {
    private static final long serialVersionUID = -100666928446877072L;

    volatile public byte elem;
    public VolatileByteRef(byte elem) { this.elem = elem; }
    public String toString() { return java.lang.Byte.toString(elem); }

    public static VolatileByteRef create(byte e) { return new VolatileByteRef(e); }
    public static VolatileByteRef zero() { return new VolatileByteRef((byte)0); }
}
