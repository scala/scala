/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction0$mcB$sp extends Function0[Any] with Serializable {
  def apply$mcB$sp: Byte
  override def apply: Any = scala.runtime.BoxesRunTime.boxToByte(apply$mcB$sp)
}