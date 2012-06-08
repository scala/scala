package scala.tools
package reflect

import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import scala.reflect.ClassTag

trait FrontEnds extends scala.reflect.api.FrontEnds {

  type Position = scala.reflect.internal.util.Position

  def mkConsoleFrontEnd(minSeverity: Int = 1): FrontEnd = {
    val settings = new Settings()
    if (minSeverity <= 0) settings.verbose.value = true
    if (minSeverity > 1) settings.nowarn.value = true
    wrapReporter(new ConsoleReporter(settings))
  }

  abstract class FrontEndToReporterProxy(val frontEnd: FrontEnd) extends AbstractReporter {
    import frontEnd.{Severity => ApiSeverity}
    val API_INFO = frontEnd.INFO
    val API_WARNING = frontEnd.WARNING
    val API_ERROR = frontEnd.ERROR

    type NscSeverity = Severity
    val NSC_INFO = INFO
    val NSC_WARNING = WARNING
    val NSC_ERROR = ERROR

    def display(pos: Position, msg: String, nscSeverity: NscSeverity): Unit =
      frontEnd.log(pos, msg, nscSeverity match {
        case NSC_INFO => API_INFO
        case NSC_WARNING => API_WARNING
        case NSC_ERROR => API_ERROR
      })

    def displayPrompt(): Unit =
      frontEnd.interactive()
  }

  def wrapFrontEnd(frontEnd: FrontEnd): Reporter = new FrontEndToReporterProxy(frontEnd) {
    val settings = new Settings()
    settings.verbose.value = true
    settings.nowarn.value = false
  }

  class ReporterToFrontEndProxy(val reporter: Reporter) extends FrontEnd {
    val API_INFO = INFO
    val API_WARNING = WARNING
    val API_ERROR = ERROR

    override def hasErrors   = reporter.hasErrors
    override def hasWarnings = reporter.hasWarnings

    def display(info: Info): Unit = info.severity match {
      case API_INFO => reporter.info(info.pos, info.msg, false)
      case API_WARNING => reporter.warning(info.pos, info.msg)
      case API_ERROR => reporter.error(info.pos, info.msg)
    }

    def interactive(): Unit = reporter match {
      case reporter: AbstractReporter => reporter.displayPrompt()
      case _ => // do nothing
    }

    override def flush(): Unit = {
      super.flush()
      reporter.flush()
    }

    override def reset(): Unit = {
      super.reset()
      reporter.reset()
    }
  }

  def wrapReporter(reporter: Reporter): FrontEnd = new ReporterToFrontEndProxy(reporter)
}
