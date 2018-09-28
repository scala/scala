/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcJF$sp extends Function1[Any, Any] with Serializable {
  def apply$mcJF$sp(v1: Float): Long
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToLong(apply$mcJF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)))
}