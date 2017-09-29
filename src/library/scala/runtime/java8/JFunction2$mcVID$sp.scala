/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction2$mcVID$sp extends Function2[Any, Any, Any] with Serializable {
  def apply$mcVID$sp(v1: Int, v2: Double): Unit
  override def apply(v1: Any, v2: Any): Any = {
    apply$mcVID$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToDouble(v2))
    scala.runtime.BoxedUnit.UNIT
  }
}