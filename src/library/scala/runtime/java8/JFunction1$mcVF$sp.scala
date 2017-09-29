/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcVF$sp extends Function1[Any, Any] with Serializable {
  def apply$mcVF$sp(v1: Float): Unit
  override def apply(t: Any): Any = {
    apply$mcVF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t))
    scala.runtime.BoxedUnit.UNIT
  }
}