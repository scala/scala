/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcJI$sp extends Function1[Any, Any] with Serializable {
  def apply$mcJI$sp(v1: Int): Long
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToLong(apply$mcJI$sp(scala.runtime.BoxesRunTime.unboxToInt(t)))
}