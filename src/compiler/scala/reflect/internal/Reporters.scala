package scala.reflect
package internal

trait Reporters { self: SymbolTable =>

  import self.{Reporter => ApiReporter}
  import scala.tools.nsc.reporters._
  import scala.tools.nsc.reporters.{Reporter => NscReporter}
  import scala.tools.nsc.Settings

  def mkConsoleReporter(minSeverity: Int = 1): ApiReporter = {
    val settings = new Settings()
    if (minSeverity <= 0) settings.verbose.value = true
    if (minSeverity > 1) settings.nowarn.value = true
    wrapNscReporter(new ConsoleReporter(settings))
  }

  abstract class ApiToNscReporterProxy(val apiReporter: ApiReporter) extends AbstractReporter {
    import apiReporter.{Severity => ApiSeverity}
    val API_INFO = apiReporter.INFO
    val API_WARNING = apiReporter.WARNING
    val API_ERROR = apiReporter.ERROR

    type NscSeverity = Severity
    val NSC_INFO = INFO
    val NSC_WARNING = WARNING
    val NSC_ERROR = ERROR

    def display(pos: Position, msg: String, nscSeverity: NscSeverity): Unit =
      apiReporter.log(pos, msg, nscSeverity match {
        case NSC_INFO => API_INFO
        case NSC_WARNING => API_WARNING
        case NSC_ERROR => API_ERROR
      })

    def displayPrompt(): Unit =
      apiReporter.interactive()
  }

  def wrapApiReporter(apiReporter: ApiReporter): NscReporter = new ApiToNscReporterProxy(apiReporter) {
    val settings = new Settings()
    settings.verbose.value = true
    settings.nowarn.value = false
  }

  class NscToApiReporterProxy(val nscReporter: NscReporter) extends ApiReporter {
    val API_INFO = INFO
    val API_WARNING = WARNING
    val API_ERROR = ERROR

    def display(info: Info): Unit = info.severity match {
      case API_INFO => nscReporter.info(info.pos, info.msg, false)
      case API_WARNING => nscReporter.warning(info.pos, info.msg)
      case API_ERROR => nscReporter.error(info.pos, info.msg)
    }

    def interactive(): Unit = nscReporter match {
      case nscReporter: AbstractReporter => nscReporter.displayPrompt()
      case _ => // do nothing
    }

    override def flush(): Unit = {
      super.flush()
      nscReporter.flush()
    }

    override def reset(): Unit = {
      super.reset()
      nscReporter.reset()
    }
  }

  def wrapNscReporter(nscReporter: NscReporter): ApiReporter = new NscToApiReporterProxy(nscReporter)
}
