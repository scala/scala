/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction2$mcDDI$sp extends Function2[Any, Any, Any] with Serializable {
  def apply$mcDDI$sp(v1: Double, v2: Int): Double
  override def apply(v1: Any, v2: Any): Any = scala.runtime.BoxesRunTime.boxToDouble(apply$mcDDI$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToInt(v2)))
}