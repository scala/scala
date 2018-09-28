/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction0$mcJ$sp extends Function0[Any] with Serializable {
  def apply$mcJ$sp: Long
  override def apply: Any = scala.runtime.BoxesRunTime.boxToLong(apply$mcJ$sp)
}