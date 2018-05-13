package scala.tools.nsc
package reporters

import scala.reflect.internal.{Reporter => InternalReporter}
import scala.reflect.internal.util.Position

/** A `FilteringReporter` that respects `-Xmaxerrs` and `-Xmaxwarns`.
 */
class LimitingReporter(settings: Settings, protected val delegate: InternalReporter) extends InternalReporter with FilteringReporter {
  override protected def filter(pos: Position, msg: String, severity: Severity) =
    severity match {
      case ERROR   => errorCount   < settings.maxerrs.value
      case WARNING => warningCount < settings.maxwarns.value
      case _       => true
    }
}
