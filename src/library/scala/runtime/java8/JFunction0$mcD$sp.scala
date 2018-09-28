/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction0$mcD$sp extends Function0[Any] with Serializable {
  def apply$mcD$sp: Double
  override def apply: Any = scala.runtime.BoxesRunTime.boxToDouble(apply$mcD$sp)
}