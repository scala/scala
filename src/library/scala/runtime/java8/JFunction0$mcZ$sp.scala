/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction0$mcZ$sp extends Function0[Any] with Serializable {
  def apply$mcZ$sp: Boolean
  override def apply: Any = scala.runtime.BoxesRunTime.boxToBoolean(apply$mcZ$sp)
}