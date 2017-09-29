/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcFJ$sp extends Function1[Any, Any] with Serializable {
  def apply$mcFJ$sp(v1: Long): Float
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToFloat(apply$mcFJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t)))
}