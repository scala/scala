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

package scala.tools.nsc.reporters

import scala.reflect.internal.util.{CodeAction, Position}
import scala.tools.nsc.Settings

/** A reporter that ignores reports.
 */
class NoReporter(val settings: Settings) extends FilteringReporter {
  override def doReport(pos: Position, msg: String, severity: Severity, actions: List[CodeAction]): Unit = ()
}
