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

package scala.math;

/** A marker class for Number types introduced by Scala
 */
public abstract class ScalaNumber extends java.lang.Number {
  protected abstract boolean isWhole();
  public abstract Object underlying();
}
