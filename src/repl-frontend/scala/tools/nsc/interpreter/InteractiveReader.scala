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

package scala.tools.nsc.interpreter

// A compatibility stub for sbt
@deprecated("Use a class in the scala.tools.nsc.interpreter package.", "2.9.0")
object InteractiveReader {
  @deprecated("Does nothing. Stub for sbt's ConsoleInterface.", "2.9.0")
  def createDefault(): Any = null
}
