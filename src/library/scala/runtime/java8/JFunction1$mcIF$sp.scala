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

@FunctionalInterface trait JFunction1$mcIF$sp extends Function1[Any, Any] with Serializable {
  def apply$mcIF$sp(v1: Float): Int
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToInteger(apply$mcIF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)))
}
