/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcIJ$sp extends Function1[Any, Any] with Serializable {
  def apply$mcIJ$sp(v1: Long): Int
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToInteger(apply$mcIJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t)))
}