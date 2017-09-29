/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction0$mcF$sp extends Function0[Any] with Serializable {
  def apply$mcF$sp: Float
  override def apply: Any = scala.runtime.BoxesRunTime.boxToFloat(apply$mcF$sp)
}