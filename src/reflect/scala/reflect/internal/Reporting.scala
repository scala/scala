/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL, Typesafe Inc.
 * @author  Adriaan Moors
 */

package scala
package reflect
package internal

import settings.MutableSettings

/** Provides delegates to the reporter doing the actual work.
 *  All forwarding methods should be marked final,
 *  but some subclasses out of our reach still override them.
 *
 *  Eventually, this interface should be reduced to one method: `reporter`,
 *  and clients should indirect themselves (reduce duplication of forwarders).
 */
trait Reporting { self : Positions =>
  def reporter: Reporter
  def currentRun: RunReporting

  trait RunReporting {
    val reporting: PerRunReporting = PerRunReporting
  }

  type PerRunReporting <: PerRunReportingBase
  protected def PerRunReporting: PerRunReporting
  abstract class PerRunReportingBase {
    def deprecationWarning(pos: Position, msg: String, since: String): Unit

    /** Have we already supplemented the error message of a compiler crash? */
    private[this] var supplementedError = false
    def supplementErrorMessage(errorMessage: String): String =
      if (supplementedError) errorMessage
      else {
        supplementedError = true
        supplementTyperState(errorMessage)
      }

  }

  // overridden in Global
  def supplementTyperState(errorMessage: String): String = errorMessage

  def supplementErrorMessage(errorMessage: String) = currentRun.reporting.supplementErrorMessage(errorMessage)

  // used by sbt of a certain vintage
  @deprecated("use reporter.echo", "2.13.0")
  def inform(msg: String): Unit      = reporter.echo(NoPosition, msg)
  @deprecated("use reporter.warning", "2.13.0")
  def warning(msg: String): Unit     = reporter.warning(NoPosition, msg)

  def abort(msg: String): Nothing = {
    val augmented = supplementErrorMessage(msg)
    // Needs to call error to make sure the compile fails.
    reporter.error(NoPosition, augmented)
    throw new FatalError(augmented)
  }
}

import util.Position

/** Report information, warnings and errors.
 *
 *  This describes the (future) external interface for issuing information, warnings and errors.
 *  Currently, scala.tools.nsc.Reporter is used by sbt/ide.
 */
abstract class Reporter {
  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit

  def echo(msg: String): Unit                   = echo(util.NoPosition, msg)
  def echo(pos: Position, msg: String): Unit    = info0(pos, msg, INFO, force = true)
  def warning(pos: Position, msg: String): Unit = info0(pos, msg, WARNING, force = false)
  def error(pos: Position, msg: String): Unit   = info0(pos, msg, ERROR, force = false)

  class Severity(val id: Int)(name: String) { var count: Int = 0 ; override def toString = name}
  object INFO    extends Severity(0)("INFO")
  object WARNING extends Severity(1)("WARNING")
  object ERROR   extends Severity(2)("ERROR")

  def count(severity: Severity): Int       = severity.count
  def resetCount(severity: Severity): Unit = severity.count = 0

  def errorCount: Int   = count(ERROR)
  def warningCount: Int = count(WARNING)

  def hasErrors: Boolean   = count(ERROR) > 0
  def hasWarnings: Boolean = count(WARNING) > 0

  def reset(): Unit = {
    resetCount(INFO)
    resetCount(WARNING)
    resetCount(ERROR)
  }

  def flush(): Unit = ()

  /** Finish reporting: print summaries, release resources. */
  def finish(): Unit = ()

  /** After reporting, offer advice on getting more details. */
  def rerunWithDetails(setting: MutableSettings#Setting, name: String): String =
    setting.value match {
      case b: Boolean if !b => s"; re-run with ${name} for details"
      case _ => s"; re-run enabling ${name} for details, or try -help"
    }
}

/** A `Reporter` that forwards all methods to a delegate.
 *
 *  Concrete subclasses must implement the abstract `delegate` member.
 */
trait ForwardingReporter extends Reporter {

  /* Receiver of all forwarded calls. */
  protected val delegate: Reporter

  /* Convenience method to forward a given message to the delegate reporter. */
  protected def forward(pos: Position, msg: String, severity: Severity): Unit =
    severity match {
      case ERROR   => delegate.error(pos, msg)
      case WARNING => delegate.warning(pos, msg)
      case INFO    => delegate.echo(pos, msg)
      case _       => throw new IllegalArgumentException(s"Unknown severity: $severity")
    }

  /* Always throws `UnsupportedOperationException`. */
  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit =
    throw new UnsupportedOperationException(s"$msg ($pos)")

  override def echo(pos: Position, msg: String)    = delegate.echo(pos, msg)
  override def warning(pos: Position, msg: String) = delegate.warning(pos, msg)
  override def error(pos: Position, msg: String)   = delegate.error(pos, msg)

  private def other(severity: Severity): delegate.Severity = severity match {
    case ERROR   => delegate.ERROR
    case WARNING => delegate.WARNING
    case _       => delegate.INFO
  }
  override def count(severity: Severity)      = delegate.count(other(severity))
  override def resetCount(severity: Severity) = delegate.resetCount(other(severity))

  override def errorCount   = delegate.errorCount
  override def warningCount = delegate.warningCount
  override def hasErrors    = delegate.hasErrors
  override def hasWarnings  = delegate.hasWarnings
  override def reset()      = delegate.reset()
  override def flush()      = delegate.flush()
  override def finish()     = delegate.finish()
  override def rerunWithDetails(setting: MutableSettings#Setting, name: String) =
                              delegate.rerunWithDetails(setting, name)
}
