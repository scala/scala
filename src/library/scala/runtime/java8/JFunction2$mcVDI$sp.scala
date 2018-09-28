/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction2$mcVDI$sp extends Function2[Any, Any, Any] with Serializable {
  def apply$mcVDI$sp(v1: Double, v2: Int): Unit
  override def apply(v1: Any, v2: Any): Any = {
    apply$mcVDI$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToInt(v2))
    scala.runtime.BoxedUnit.UNIT
  }
}