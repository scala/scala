/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcDI$sp extends Function1[Any, Any] with Serializable {
  def apply$mcDI$sp(v1: Int): Double
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToDouble(apply$mcDI$sp(scala.runtime.BoxesRunTime.unboxToInt(t)))
}