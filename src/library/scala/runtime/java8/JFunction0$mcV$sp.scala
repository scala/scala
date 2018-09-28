/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.runtime.java8

@FunctionalInterface trait JFunction0$mcV$sp extends Function0[Any] with Serializable {
  def apply$mcV$sp(): Unit
  override def apply: Any = {
    apply$mcV$sp()
    scala.runtime.BoxedUnit.UNIT
  }
}