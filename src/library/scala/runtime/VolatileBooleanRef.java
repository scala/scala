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

public final class VolatileBooleanRef implements java.io.Serializable {
    private static final long serialVersionUID = -5730524563015615974L;

    volatile public boolean elem;
    public VolatileBooleanRef(boolean elem) { this.elem = elem; }
    public String toString() { return String.valueOf(elem); }

    public static VolatileBooleanRef create(boolean e) { return new VolatileBooleanRef(e); }
    public static VolatileBooleanRef zero() { return new VolatileBooleanRef(false); }
}
