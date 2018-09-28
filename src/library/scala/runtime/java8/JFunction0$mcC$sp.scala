/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction0$mcC$sp extends Function0[Any] with Serializable {
  def apply$mcC$sp: Char
  override def apply: Any = scala.runtime.BoxesRunTime.boxToCharacter(apply$mcC$sp)
}