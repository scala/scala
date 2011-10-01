/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

package object target {
  @deprecated("Use `@scala.beans.meta.beanGetter` instead", "2.10.0")
  type beanGetter = scala.beans.meta.beanGetter

  @deprecated("Use `@scala.beans.meta.beanSetter` instead", "2.10.0")
  type beanSetter = scala.beans.meta.beanSetter

  @deprecated("Use `@scala.beans.meta.field` instead", "2.10.0")
  type field = scala.beans.meta.field

  @deprecated("Use `@scala.beans.meta.getter` instead", "2.10.0")
  type getter = scala.beans.meta.getter

  @deprecated("Use `@scala.beans.meta.param` instead", "2.10.0")
  type param = scala.beans.meta.param

  @deprecated("Use `@scala.beans.meta.setter` instead", "2.10.0")
  type setter = scala.beans.meta.setter
}
