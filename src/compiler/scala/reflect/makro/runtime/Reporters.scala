package scala.reflect.makro
package runtime

trait Reporters {
  self: Context =>

  import mirror._

  def reporter: mirror.Reporter = wrapNscReporter(mirror.reporter)

  def setReporter(reporter: mirror.Reporter): this.type = {
    mirror.reporter = wrapApiReporter(reporter)
    this
  }

  def withReporter[T](reporter: Reporter)(op: => T): T = {
    val old = mirror.reporter
    setReporter(reporter)
    try op
    finally mirror.reporter = old
  }

  def echo(pos: Position, msg: String): Unit = mirror.reporter.echo(pos, msg)

  def info(pos: Position, msg: String, force: Boolean): Unit = mirror.reporter.info(pos, msg, force)

  def hasWarnings: Boolean = mirror.reporter.hasErrors

  def hasErrors: Boolean = mirror.reporter.hasErrors

  def warning(pos: Position, msg: String): Unit = callsiteTyper.context.warning(pos, msg)

  def error(pos: Position, msg: String): Unit = callsiteTyper.context.error(pos, msg)

  def abort(pos: Position, msg: String): Nothing = {
    callsiteTyper.context.error(pos, msg)
    throw new AbortMacroException(pos, msg)
  }

  def interactive(): Unit = mirror.reporter match {
    case reporter: tools.nsc.reporters.AbstractReporter => reporter.displayPrompt()
    case _ => ()
  }
}