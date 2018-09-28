/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcJD$sp extends Function1[Any, Any] with Serializable {
  def apply$mcJD$sp(v1: Double): Long
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToLong(apply$mcJD$sp(scala.runtime.BoxesRunTime.unboxToDouble(t)))
}