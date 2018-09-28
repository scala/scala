/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction2$mcFIJ$sp extends Function2[Any, Any, Any] with Serializable {
  def apply$mcFIJ$sp(v1: Int, v2: Long): Float
  override def apply(v1: Any, v2: Any): Any = scala.runtime.BoxesRunTime.boxToFloat(apply$mcFIJ$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)))
}