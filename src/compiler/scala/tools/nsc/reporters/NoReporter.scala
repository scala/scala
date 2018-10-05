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

package scala.tools.nsc.reporters

import scala.reflect.internal.util.Position

/** A reporter that ignores reports.
 */
object NoReporter extends Reporter {
  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = ()
}
