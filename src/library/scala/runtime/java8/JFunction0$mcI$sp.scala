/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction0$mcI$sp extends Function0[Any] with Serializable {
  def apply$mcI$sp: Int
  override def apply: Any = scala.runtime.BoxesRunTime.boxToInteger(apply$mcI$sp)
}