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
  val decorateAsJava  = new DecorateAsJava { }
  val decorateAsScala = new DecorateAsScala { }
  val decorateAll     = new DecorateAsJava with DecorateAsScala { }
  val wrapAsJava      = new WrapAsJava { }
  val wrapAsScala     = new WrapAsScala { }
  val wrapAll         = new WrapAsJava with WrapAsScala { }
}
