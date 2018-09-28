/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcZD$sp extends Function1[Any, Any] with Serializable {
  def apply$mcZD$sp(v1: Double): Boolean
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToBoolean(apply$mcZD$sp(scala.runtime.BoxesRunTime.unboxToDouble(t)))
}