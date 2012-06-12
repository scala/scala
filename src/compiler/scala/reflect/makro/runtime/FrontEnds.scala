package scala.reflect.makro
package runtime

trait FrontEnds extends scala.tools.reflect.FrontEnds {
  self: Context =>

  import universe._
  import mirror._

  override type Position = universe.Position

  def frontEnd: FrontEnd = wrapReporter(universe.reporter)

  def setFrontEnd(frontEnd: FrontEnd): this.type = {
    universe.reporter = wrapFrontEnd(frontEnd)
    this
  }

  def withFrontEnd[T](frontEnd: FrontEnd)(op: => T): T = {
    val old = universe.reporter
    setFrontEnd(frontEnd)
    try op
    finally universe.reporter = old
  }

  def echo(pos: Position, msg: String): Unit = universe.reporter.echo(pos, msg)

  def info(pos: Position, msg: String, force: Boolean): Unit = universe.reporter.info(pos, msg, force)

  def hasWarnings: Boolean = universe.reporter.hasErrors

  def hasErrors: Boolean = universe.reporter.hasErrors

  def warning(pos: Position, msg: String): Unit = callsiteTyper.context.warning(pos, msg)

  def error(pos: Position, msg: String): Unit = callsiteTyper.context.error(pos, msg)

  def abort(pos: Position, msg: String): Nothing = {
    callsiteTyper.context.error(pos, msg)
    throw new AbortMacroException(pos, msg)
  }

  def interactive(): Unit = universe.reporter match {
    case reporter: tools.nsc.reporters.AbstractReporter => reporter.displayPrompt()
    case _ => ()
  }
}