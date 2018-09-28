/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcFF$sp extends Function1[Any, Any] with Serializable {
  def apply$mcFF$sp(v1: Float): Float
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToFloat(apply$mcFF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)))
}