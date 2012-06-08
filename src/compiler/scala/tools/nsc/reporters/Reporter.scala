/* NSC -- new Scala compiler
 * Copyright 2002-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package reporters

import scala.reflect.internal.util._
import scala.reflect.internal.util.StringOps._

/**
 * This interface provides methods to issue information, warning and
 * error messages.
 */
abstract class Reporter {
  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit

  object severity extends Enumeration
  class Severity(val id: Int) extends severity.Value {
    var count: Int = 0
  }
  val INFO    = new Severity(0)
  val WARNING = new Severity(1)
  val ERROR   = new Severity(2)

  /** Whether very long lines can be truncated.  This exists so important
   *  debugging information (like printing the classpath) is not rendered
   *  invisible due to the max message length.
   */
  private var _truncationOK: Boolean = true
  def truncationOK = _truncationOK
  def withoutTruncating[T](body: => T): T = {
    val saved = _truncationOK
    _truncationOK = false
    try body
    finally _truncationOK = saved
  }

  private var incompleteHandler: (Position, String) => Unit = null
  def incompleteHandled = incompleteHandler != null
  def withIncompleteHandler[T](handler: (Position, String) => Unit)(thunk: => T) = {
    val saved = incompleteHandler
    incompleteHandler = handler
    try thunk
    finally incompleteHandler = saved
  }

  var cancelled   = false
  def hasErrors   = ERROR.count > 0 || cancelled
  def hasWarnings = WARNING.count > 0

  /** For sending a message which should not be labeled as a warning/error,
   *  but also shouldn't require -verbose to be visible.
   */
  def echo(msg: String): Unit                                = info(NoPosition, msg, true)
  def echo(pos: Position, msg: String): Unit                 = info(pos, msg, true)

  /** Informational messages, suppressed unless -verbose or force=true. */
  def info(pos: Position, msg: String, force: Boolean): Unit = info0(pos, msg, INFO, force)

  /** Warnings and errors. */
  def warning(pos: Position, msg: String): Unit              = withoutTruncating(info0(pos, msg, WARNING, false))
  def error(pos: Position, msg: String): Unit                = withoutTruncating(info0(pos, msg, ERROR, false))
  def incompleteInputError(pos: Position, msg: String): Unit = {
    if (incompleteHandled) incompleteHandler(pos, msg)
    else error(pos, msg)
  }

  def comment(pos: Position, msg: String) { }
  def flush() { }
  def reset() {
    INFO.count        = 0
    ERROR.count       = 0
    WARNING.count     = 0
    cancelled         = false
  }

  // sbt compat
  @deprecated("Moved to scala.reflect.internal.util.StringOps", "2.10.0")
  def countElementsAsString(n: Int, elements: String): String = StringOps.countElementsAsString(n, elements)
  @deprecated("Moved to scala.reflect.internal.util.StringOps", "2.10.0")
  def countAsString(n: Int): String = StringOps.countAsString(n)
}
