/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction2$mcDJI$sp extends Function2[Any, Any, Any] with Serializable {
  def apply$mcDJI$sp(v1: Long, v2: Int): Double
  override def apply(v1: Any, v2: Any): Any = scala.runtime.BoxesRunTime.boxToDouble(apply$mcDJI$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToInt(v2)))
}