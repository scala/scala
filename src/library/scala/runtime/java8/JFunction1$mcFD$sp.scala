/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcFD$sp extends Function1[Any, Any] with Serializable {
  def apply$mcFD$sp(v1: Double): Float
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToFloat(apply$mcFD$sp(scala.runtime.BoxesRunTime.unboxToDouble(t)))
}