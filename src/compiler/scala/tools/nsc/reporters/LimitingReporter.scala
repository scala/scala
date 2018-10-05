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
package reporters

import scala.reflect.internal.{Reporter => InternalReporter, FilteringReporter}
import scala.reflect.internal.util.Position

/** A `Filter` that respects `-Xmaxerrs` and `-Xmaxwarns`.
 */
class LimitingReporter(settings: Settings, override protected val delegate: InternalReporter) extends Reporter with FilteringReporter {
  override protected def filter(pos: Position, msg: String, severity: Severity) =
    severity match {
      case ERROR   => errorCount   < settings.maxerrs.value
      case WARNING => warningCount < settings.maxwarns.value
      case _       => true
    }
  // work around fractured API to support `reporters.Reporter.info`
  override protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = delegate match {
    case r: Reporter =>
      severity match {
        case ERROR   => r.error(pos, msg)
        case WARNING => r.warning(pos, msg)
        case _       => if (force) r.echo(pos, msg) else r.info(pos, msg, force = false)
      }
    case _           => super.info0(pos, msg, severity, force)
  }
}
