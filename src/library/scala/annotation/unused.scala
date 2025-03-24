/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.annotation

/** Mark an element unused for a given context.
 *
 *  Unused warnings are suppressed for elements known to be unused.
 *
 *  For example, a method parameter may be marked `@unused`
 *  because the method is designed to be overridden by
 *  an implementation that does use the parameter.
 */
@meta.getter @meta.setter
class unused(message: String) extends StaticAnnotation {
  def this() = this("")
}
