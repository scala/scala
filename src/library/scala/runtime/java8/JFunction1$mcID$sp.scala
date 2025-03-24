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

package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcID$sp extends Function1[Any, Any] with Serializable {
  def apply$mcID$sp(v1: Double): Int
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToInteger(apply$mcID$sp(scala.runtime.BoxesRunTime.unboxToDouble(t)))
}
