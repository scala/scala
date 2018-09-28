/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcDJ$sp extends Function1[Any, Any] with Serializable {
  def apply$mcDJ$sp(v1: Long): Double
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToDouble(apply$mcDJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t)))
}