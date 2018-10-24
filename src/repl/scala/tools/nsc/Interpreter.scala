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

import interpreter._
import java.io._

/** A compatibility stub.
 */
@deprecated("Use a class in the scala.tools.nsc.interpreter package.", "2.9.0")
class Interpreter(settings: Settings, out: PrintWriter) extends IMain(settings, out) {
  def this(settings: Settings) = this(settings, new NewLinePrintWriter(new ConsoleWriter, true))
  def this() = this(new Settings())
}