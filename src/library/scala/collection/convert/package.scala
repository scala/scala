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
  @deprecated("use JavaConverters", since="2.12.0")
  val decorateAsJava  = new DecorateAsJava { }
  @deprecated("use JavaConverters", since="2.12.0")
  val decorateAsScala = new DecorateAsScala { }
  @deprecated("use JavaConverters", since="2.12.0")
  val decorateAll     = JavaConverters

  @deprecated("use JavaConverters or consider ImplicitConversionsToJava", since="2.12.0")
  val wrapAsJava      = new WrapAsJava { }
  @deprecated("use JavaConverters or consider ImplicitConversionsToScala", since="2.12.0")
  val wrapAsScala     = new WrapAsScala { }
  @deprecated("use JavaConverters or consider ImplicitConversions", since="2.12.0")
  val wrapAll         = new WrapAsJava with WrapAsScala { }
}
