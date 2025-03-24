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

public final class ShortRef implements java.io.Serializable {
    private static final long serialVersionUID = 4218441291229072313L;

    public short elem;
    public ShortRef(short elem) { this.elem = elem; }
    public String toString() { return java.lang.Short.toString(elem); }

    public static ShortRef create(short e) { return new ShortRef(e); }
    public static ShortRef zero() { return new ShortRef((short)0); }
}
