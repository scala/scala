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

public final class LongRef implements java.io.Serializable {
    private static final long serialVersionUID = -3567869820105829499L;

    public long elem;
    public LongRef(long elem) { this.elem = elem; }
    public String toString() { return java.lang.Long.toString(elem); }

    public static LongRef create(long e) { return new LongRef(e); }
    public static LongRef zero() { return new LongRef(0); }
}
