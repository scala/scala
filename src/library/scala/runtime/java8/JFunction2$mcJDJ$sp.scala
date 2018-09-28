/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction2$mcJDJ$sp extends Function2[Any, Any, Any] with Serializable {
  def apply$mcJDJ$sp(v1: Double, v2: Long): Long
  override def apply(v1: Any, v2: Any): Any = scala.runtime.BoxesRunTime.boxToLong(apply$mcJDJ$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)))
}