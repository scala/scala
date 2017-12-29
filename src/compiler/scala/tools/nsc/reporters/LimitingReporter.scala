package scala.tools.nsc
package reporters

// TODO
//import scala.reflect.internal.Reporter
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
}
