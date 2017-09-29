/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcFI$sp extends Function1[Any, Any] with Serializable {
  def apply$mcFI$sp(v1: Int): Float
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToFloat(apply$mcFI$sp(scala.runtime.BoxesRunTime.unboxToInt(t)))
}