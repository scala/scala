package scala.tools.nsc.reporters

import scala.reflect.internal.util.Position
// TODO
//import scala.reflect.internal.Reporter

/**
  * A reporter that ignores reports.
  */
object NoReporter extends Reporter {
  override protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = ()
}
