/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction0$mcS$sp extends Function0[Any] with Serializable {
  def apply$mcS$sp: Short
  override def apply: Any = scala.runtime.BoxesRunTime.boxToShort(apply$mcS$sp)
}