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

@FunctionalInterface trait JFunction1$mcVI$sp extends Function1[Any, Any] with Serializable {
  def apply$mcVI$sp(v1: Int): Unit
  override def apply(t: Any): Any = {
    apply$mcVI$sp(scala.runtime.BoxesRunTime.unboxToInt(t))
    scala.runtime.BoxedUnit.UNIT
  }
}
