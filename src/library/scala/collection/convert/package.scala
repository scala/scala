/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

package object convert {
  @deprecated("Use JavaConverters", since="2.12")
  val decorateAsJava  = new DecorateAsJava { }
  @deprecated("Use JavaConverters", since="2.12")
  val decorateAsScala = new DecorateAsScala { }
  @deprecated("Use JavaConverters", since="2.12")
  val decorateAll     = JavaConverters

  @deprecated("Use JavaConverters or consider ImplicitConversionsToJava", since="2.12")
  val wrapAsJava      = new WrapAsJava { }
  @deprecated("Use JavaConverters or consider ImplicitConversionsToScala", since="2.12")
  val wrapAsScala     = new WrapAsScala { }
  @deprecated("Use JavaConverters or consider ImplicitConversions", since="2.12")
  val wrapAll         = new WrapAsJava with WrapAsScala { }
}
