/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction1$mcVI$sp extends Function1[Any, Any] with Serializable {
  def apply$mcVI$sp(v1: Int): Unit
  override def apply(t: Any): Any = {
    apply$mcVI$sp(scala.runtime.BoxesRunTime.unboxToInt(t))
    scala.runtime.BoxedUnit.UNIT
  }
}