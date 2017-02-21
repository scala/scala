/* NSC -- new Scala compiler
 * Copyright 2002-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import reporters._
import IMain._

import scala.reflect.internal.util.Position

/** Like ReplGlobal, a layer for ensuring extra functionality.
 */
class ReplReporter(intp: IMain) extends ConsoleReporter(intp.settings, Console.in, new ReplStrippingWriter(intp)) {
  def printUntruncatedMessage(msg: String) = withoutTruncating(printMessage(msg))

  /** Whether very long lines can be truncated.  This exists so important
   *  debugging information (like printing the classpath) is not rendered
   *  invisible due to the max message length.
   */
  private var _truncationOK: Boolean = !intp.settings.verbose
  def truncationOK = _truncationOK
  def withoutTruncating[T](body: => T): T = {
    val saved = _truncationOK
    _truncationOK = false
    try body
    finally _truncationOK = saved
  }

  override def warning(pos: Position, msg: String): Unit = withoutTruncating(super.warning(pos, msg))
  override def error(pos: Position, msg: String): Unit   = withoutTruncating(super.error(pos, msg))

  import scala.io.AnsiColor.{ RED, YELLOW, RESET }

  def severityColor(severity: Severity): String = severity match {
    case ERROR   => RED
    case WARNING => YELLOW
    case INFO    => RESET
  }

  override def print(pos: Position, msg: String, severity: Severity) {
    val prefix = (
      if (replProps.colorOk)
        severityColor(severity) + clabel(severity) + RESET
      else
        clabel(severity)
    )
    printMessage(pos, prefix + msg)
  }

  override def printMessage(msg: String) {
    // Avoiding deadlock if the compiler starts logging before
    // the lazy val is complete.
    if (intp.isInitializeComplete) {
      if (intp.totalSilence) {
        if (isReplTrace)
          super.printMessage("[silent] " + msg)
      }
      else super.printMessage(msg)
    }
    else Console.println("[init] " + msg)
  }

  override def displayPrompt() {
    if (intp.totalSilence) ()
    else super.displayPrompt()
  }

  override def rerunWithDetails(setting: reflect.internal.settings.MutableSettings#Setting, name: String) =
    s"; for details, enable `:setting $name' or `:replay $name'"

}
