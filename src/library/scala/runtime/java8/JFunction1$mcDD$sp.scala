/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcDD$sp extends Function1[Any, Any] with Serializable {
  def apply$mcDD$sp(v1: Double): Double
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToDouble(apply$mcDD$sp(scala.runtime.BoxesRunTime.unboxToDouble(t)))
}