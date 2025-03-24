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

public final class CharRef implements java.io.Serializable {
    private static final long serialVersionUID = 6537214938268005702L;

    public char elem;
    public CharRef(char elem) { this.elem = elem; }
    public String toString() { return java.lang.Character.toString(elem); }

    public static CharRef create(char e) { return new CharRef(e); }
    public static CharRef zero() { return new CharRef((char)0); }
}
