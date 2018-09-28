/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction2$mcJIJ$sp extends Function2[Any, Any, Any] with Serializable {
  def apply$mcJIJ$sp(v1: Int, v2: Long): Long
  override def apply(v1: Any, v2: Any): Any = scala.runtime.BoxesRunTime.boxToLong(apply$mcJIJ$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)))
}