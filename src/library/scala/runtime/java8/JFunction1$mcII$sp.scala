/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcII$sp extends Function1[Any, Any] with Serializable {
  def apply$mcII$sp(v1: Int): Int
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToInteger(apply$mcII$sp(scala.runtime.BoxesRunTime.unboxToInt(t)))
}