/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

package object target {
  @deprecated("Use `@scala.annotation.meta.beanGetter` instead", "2.10.0")
  type beanGetter = scala.annotation.meta.beanGetter

  @deprecated("Use `@scala.annotation.meta.beanSetter` instead", "2.10.0")
  type beanSetter = scala.annotation.meta.beanSetter

  @deprecated("Use `@scala.annotation.meta.field` instead", "2.10.0")
  type field = scala.annotation.meta.field

  @deprecated("Use `@scala.annotation.meta.getter` instead", "2.10.0")
  type getter = scala.annotation.meta.getter

  @deprecated("Use `@scala.annotation.meta.param` instead", "2.10.0")
  type param = scala.annotation.meta.param

  @deprecated("Use `@scala.annotation.meta.setter` instead", "2.10.0")
  type setter = scala.annotation.meta.setter
}
