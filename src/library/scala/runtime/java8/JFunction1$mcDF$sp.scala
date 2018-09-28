/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcDF$sp extends Function1[Any, Any] with Serializable {
  def apply$mcDF$sp(v1: Float): Double
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToDouble(apply$mcDF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)))
}