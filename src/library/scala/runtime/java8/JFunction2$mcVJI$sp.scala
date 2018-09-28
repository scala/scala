/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction2$mcVJI$sp extends Function2[Any, Any, Any] with Serializable {
  def apply$mcVJI$sp(v1: Long, v2: Int): Unit
  override def apply(v1: Any, v2: Any): Any = {
    apply$mcVJI$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToInt(v2))
    scala.runtime.BoxedUnit.UNIT
  }
}