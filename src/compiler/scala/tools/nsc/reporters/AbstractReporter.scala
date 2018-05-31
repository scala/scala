/* NSC -- new Scala compiler
 * Copyright 2002-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package reporters

import scala.collection.mutable
import scala.tools.nsc.Settings
import scala.reflect.internal.util.Position

/** This reporter uses filtering by severity and position,
 *  including two-tiered INFO messaging,
 *  and also handles prompting and debug output.
 */
@deprecated("Use PositionFiltering", since="2.13")
abstract class AbstractReporter extends Reporter with PositionFiltering {
  val settings: Settings
  def display(pos: Position, msg: String, severity: Severity): Unit
  def displayPrompt(): Unit

  protected def noWarnings = settings.nowarnings.value
  private def isVerbose    = settings.verbose.value
  private def isPromptSet  = settings.prompt.value
  private def isDebug      = settings.debug.value

  protected def suppressed(pos: Position, msg: String, severity: Severity): Unit =
    if (isPromptSet)  countAndDisplay(pos, msg, severity)
    else if (isDebug) countAndDisplay(pos, "[ suppressed ] " + msg, severity)

  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit =
    if (filter(pos, msg, severity) && (severity != INFO || (force || isVerbose)))
      countAndDisplay(pos, msg, severity)

  private def countAndDisplay(pos: Position, msg: String, severity: Severity): Unit = {
    severity.count += 1
    display(pos, msg, severity)
    if (isPromptSet && severity != INFO) displayPrompt()
  }
}
