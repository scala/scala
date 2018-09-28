/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcJJ$sp extends Function1[Any, Any] with Serializable {
  def apply$mcJJ$sp(v1: Long): Long
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToLong(apply$mcJJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t)))
}