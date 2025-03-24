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

public final class BooleanRef implements java.io.Serializable {
    private static final long serialVersionUID = -5730524563015615974L;

    public boolean elem;
    public BooleanRef(boolean elem) { this.elem = elem; }
    public String toString() { return String.valueOf(elem); }

    public static BooleanRef create(boolean e) { return new BooleanRef(e); }
    public static BooleanRef zero() { return new BooleanRef(false); }
}
