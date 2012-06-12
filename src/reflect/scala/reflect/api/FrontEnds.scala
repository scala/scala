package scala.reflect
package api

// [Martin to Eugene] Todo: Needs to be evicted from API
// [Eugene++ to Martin] but how? we need them for macros
trait FrontEnds {

  type Position >: Null

  trait FrontEnd {
    object severity extends Enumeration
    class Severity(val id: Int) extends severity.Value {
      var count: Int = 0
      override def toString() = this match {
        case INFO => "INFO"
        case WARNING => "WARNING"
        case ERROR => "ERROR"
        case _ => "<unknown>"
      }
    }
    val INFO    = new Severity(0)
    val WARNING = new Severity(1)
    val ERROR   = new Severity(2)

    def hasErrors   = ERROR.count > 0
    def hasWarnings = WARNING.count > 0

    case class Info(val pos: Position, val msg: String, val severity: Severity)
    val infos = new collection.mutable.LinkedHashSet[Info]

    /** Handles incoming info */
    def log(pos: Position, msg: String, severity: Severity) {
      infos += new Info(pos, msg, severity)
      severity.count += 1
      display(infos.last)
    }

    /** Displays incoming info */
    def display(info: Info): Unit

    /** Services a request to drop into interactive mode */
    def interactive(): Unit

    /** Refreshes the UI */
    def flush(): Unit = {}

    /** Resets the reporter */
    def reset(): Unit = {
      INFO.count = 0
      WARNING.count = 0
      ERROR.count = 0
      infos.clear()
    }
  }

  class SilentFrontEnd extends FrontEnd {
    def display(info: Info) {}
    def interactive() {}
  }

  /** Creates a UI-less reporter that simply accumulates all the messages
   */
  def mkSilentFrontEnd(): FrontEnd = new SilentFrontEnd()

  /** Creates a reporter that prints messages to the console according to the settings.
   *
   *  ``minSeverity'' determines minimum severity of the messages to be printed.
   *  0 stands for INFO, 1 stands for WARNING and 2 stands for ERROR.
   */
  // todo. untangle warningsAsErrors from Reporters. I don't feel like moving this flag here!
  def mkConsoleFrontEnd(minSeverity: Int = 1): FrontEnd
}