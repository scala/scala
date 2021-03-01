/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package interpreter

import scala.reflect.internal.Reporter
import reporters.ConsoleReporter
import IMain._

import scala.reflect.internal.util.{OffsetPosition, Position}

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

  private[this] var silentErrors = 0
  override def warning(pos: Position, msg: String): Unit = withoutTruncating(super.warning(pos, msg))
  override def error(pos: Position, msg: String): Unit   = {
    val count0 = errorCount
    withoutTruncating(super.error(pos, msg))
    val count1 = errorCount
    if (count1 > count0 && intp.totalSilence)
      silentErrors += (count1 - count0)
  }
  private[interpreter] def reportableErrorCount = errorCount - silentErrors
  private[interpreter] def hasReportableErrors  = reportableErrorCount > 0

  override def reset(): Unit = {
    silentErrors = 0
    super.reset()
  }

  import scala.io.AnsiColor.{RED, YELLOW, RESET}

  def severityColor(severity: Severity): String = severity match {
    case Reporter.ERROR   => RED
    case Reporter.WARNING => YELLOW
    case Reporter.INFO    => RESET
  }

  private val promptLength = replProps.promptText.linesIterator.toList.last.length
  private val indentation  = " " * promptLength

  // colorized console labels
  override protected def clabel(severity: Severity): String = {
    val label0 = super.clabel(severity)
    if (replProps.colorOk) s"${severityColor(severity)}${label0}${RESET}" else label0
  }

  // shift indentation for source text entered at prompt
  override protected def display(pos: Position, msg: String, severity: Severity): Unit = {
    val adjusted =
      if (pos.source.file.name == "<console>")
        new OffsetPosition(pos.source, pos.offset.getOrElse(0)) {
          override def lineContent = s"${indentation}${super.lineContent}"
          override def lineCaret   = s"${indentation}${super.lineCaret}"
        }
      else pos
    super.display(adjusted, msg, severity)
  }

  override def printMessage(msg: String): Unit = {
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

  override def displayPrompt(): Unit = if (!intp.totalSilence) super.displayPrompt()

  override def rerunWithDetails(setting: reflect.internal.settings.MutableSettings#Setting, name: String) =
    s"; for details, enable `:setting $name' or `:replay $name'"
}
