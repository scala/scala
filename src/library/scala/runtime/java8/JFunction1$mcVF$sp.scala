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

@FunctionalInterface trait JFunction1$mcVF$sp extends Function1[Any, Any] with Serializable {
  def apply$mcVF$sp(v1: Float): Unit
  override def apply(t: Any): Any = {
    apply$mcVF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t))
    scala.runtime.BoxedUnit.UNIT
  }
}
