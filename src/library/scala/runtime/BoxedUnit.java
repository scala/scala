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


public final class BoxedUnit implements java.io.Serializable {
    private static final long serialVersionUID = 8405543498931817370L;

    public final static BoxedUnit UNIT = new BoxedUnit();

    public final static Class<Void> TYPE = java.lang.Void.TYPE;
    
    private Object readResolve() { return UNIT; }

    private BoxedUnit() { }

    public boolean equals(java.lang.Object other) {
	return this == other;
    }

    public int hashCode() {
	return 0;
    }

    public String toString() {
	return "()";
    }
}
