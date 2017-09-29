/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcIF$sp extends Function1[Any, Any] with Serializable {
  def apply$mcIF$sp(v1: Float): Int
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToInteger(apply$mcIF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)))
}