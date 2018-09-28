/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction2$mcDII$sp extends Function2[Any, Any, Any] with Serializable {
  def apply$mcDII$sp(v1: Int, v2: Int): Double
  override def apply(v1: Any, v2: Any): Any = scala.runtime.BoxesRunTime.boxToDouble(apply$mcDII$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToInt(v2)))
}