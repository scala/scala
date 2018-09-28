/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction2$mcIJJ$sp extends Function2[Any, Any, Any] with Serializable {
  def apply$mcIJJ$sp(v1: Long, v2: Long): Int
  override def apply(v1: Any, v2: Any): Any = scala.runtime.BoxesRunTime.boxToInteger(apply$mcIJJ$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)))
}