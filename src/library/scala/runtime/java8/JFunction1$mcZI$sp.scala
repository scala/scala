/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcZI$sp extends Function1[Any, Any] with Serializable {
  def apply$mcZI$sp(v1: Int): Boolean
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToBoolean(apply$mcZI$sp(scala.runtime.BoxesRunTime.unboxToInt(t)))
}