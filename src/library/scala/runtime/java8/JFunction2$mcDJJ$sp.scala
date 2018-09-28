/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction2$mcDJJ$sp extends Function2[Any, Any, Any] with Serializable {
  def apply$mcDJJ$sp(v1: Long, v2: Long): Double
  override def apply(v1: Any, v2: Any): Any = scala.runtime.BoxesRunTime.boxToDouble(apply$mcDJJ$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)))
}