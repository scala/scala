package scala.tools.nsc.reporters

import scala.reflect.internal.util.Position

/** A reporter that ignores reports.
 *
 *  It should probably be called RudeReporter.
 */
object NoReporter extends Reporter {
  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = ()
}
