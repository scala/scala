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

package scala.tools.nsc
package interpreter

/** A command line for the interpreter.
 */
class CommandLine(arguments: List[String], error: String => Unit) extends CompilerCommand(arguments, error) {
  override def cmdName = "scala"
}
