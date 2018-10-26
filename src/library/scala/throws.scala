/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

/**
 * Annotation for specifying the exceptions thrown by a method.
 * For example:
 * {{{
 * class Reader(fname: String) {
 *   private val in = new BufferedReader(new FileReader(fname))
 *   @throws[IOException]("if the file doesn't exist")
 *   def read() = in.read()
 * }
 * }}}
 *
 * @author  Nikolay Mihaylov
 * @since   2.1
 */
class throws[T <: Throwable](cause: String = "") extends scala.annotation.StaticAnnotation {
  def this(clazz: Class[T]) = this("")
}
