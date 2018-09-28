/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcZF$sp extends Function1[Any, Any] with Serializable {
  def apply$mcZF$sp(v1: Float): Boolean
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToBoolean(apply$mcZF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)))
}