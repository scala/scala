/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcVJ$sp extends Function1[Any, Any] with Serializable {
  def apply$mcVJ$sp(v1: Long): Unit
  override def apply(t: Any): Any = {
    apply$mcVJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t))
    scala.runtime.BoxedUnit.UNIT
  }
}