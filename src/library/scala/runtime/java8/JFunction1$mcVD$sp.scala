/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcVD$sp extends Function1[Any, Any] with Serializable {
  def apply$mcVD$sp(v1: Double): Unit
  override def apply(t: Any): Any = {
    apply$mcVD$sp(scala.runtime.BoxesRunTime.unboxToDouble(t))
    scala.runtime.BoxedUnit.UNIT
  }
}