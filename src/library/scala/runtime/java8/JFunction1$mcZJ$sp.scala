/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcZJ$sp extends Function1[Any, Any] with Serializable {
  def apply$mcZJ$sp(v1: Long): Boolean
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToBoolean(apply$mcZJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t)))
}