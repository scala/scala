package scala.reflect.macros
package contexts

import scala.reflect.macros.runtime.AbortMacroException

trait FrontEnds {
  self: Context =>

  def echo(pos: Position, msg: String): Unit = universe.reporter.echo(pos, msg)

  @deprecated("Use echo, info messages are always forced", since="2.13.0")
  def info(pos: Position, msg: String, force: Boolean): Unit = universe.reporter.echo(pos, msg)

  def hasWarnings: Boolean = universe.reporter.hasErrors

  def hasErrors: Boolean = universe.reporter.hasErrors

  def warning(pos: Position, msg: String): Unit = callsiteTyper.context.warning(pos, msg)

  def error(pos: Position, msg: String): Unit = callsiteTyper.context.error(pos, msg)

  def abort(pos: Position, msg: String): Nothing = throw new AbortMacroException(pos, msg)
}
