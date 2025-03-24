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

public final class ByteRef implements java.io.Serializable {
    private static final long serialVersionUID = -100666928446877072L;

    public byte elem;
    public ByteRef(byte elem) { this.elem = elem; }
    public String toString() { return java.lang.Byte.toString(elem); }

    public static ByteRef create(byte e) { return new ByteRef(e); }
    public static ByteRef zero() { return new ByteRef((byte)0); }
}
