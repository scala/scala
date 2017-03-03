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

  @deprecatedOverriding("This forwards to the corresponding method in reporter -- override reporter instead", "2.11.2")
  def inform(msg: String): Unit      = inform(NoPosition, msg)
  @deprecatedOverriding("This forwards to the corresponding method in reporter -- override reporter instead", "2.11.2")
  def warning(msg: String): Unit     = warning(NoPosition, msg)
  // globalError(msg: String) used to abort -- not sure that was a good idea, so I made it more regular
  // (couldn't find any uses that relied on old behavior)
  @deprecatedOverriding("This forwards to the corresponding method in reporter -- override reporter instead", "2.11.2")
  def globalError(msg: String): Unit = globalError(NoPosition, msg)

  def abort(msg: String): Nothing = {
    val augmented = supplementErrorMessage(msg)
    // Needs to call error to make sure the compile fails.
    globalError(augmented)
    throw new FatalError(augmented)
  }

  @deprecatedOverriding("This forwards to the corresponding method in reporter -- override reporter instead", "2.11.2")
  def inform(pos: Position, msg: String)      = reporter.echo(pos, msg)
  @deprecatedOverriding("This forwards to the corresponding method in reporter -- override reporter instead", "2.11.2")
  def warning(pos: Position, msg: String)     = reporter.warning(pos, msg)
  @deprecatedOverriding("This forwards to the corresponding method in reporter -- override reporter instead", "2.11.2")
  def globalError(pos: Position, msg: String) = reporter.error(pos, msg)
}

import util.Position

/** Report information, warnings and errors.
 *
 *  This describes the (future) external interface for issuing information, warnings and errors.
 *  Currently, scala.tools.nsc.Reporter is used by sbt/ide/partest.
 */
abstract class Reporter {
  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit

  def echo(pos: Position, msg: String): Unit    = info0(pos, msg, INFO, force = true)
  def warning(pos: Position, msg: String): Unit = info0(pos, msg, WARNING, force = false)
  def error(pos: Position, msg: String): Unit   = info0(pos, msg, ERROR, force = false)

  type Severity
  val INFO: Severity
  val WARNING: Severity
  val ERROR: Severity

  def count(severity: Severity): Int
  def resetCount(severity: Severity): Unit

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

// TODO: move into superclass once partest cuts tie on Severity
abstract class ReporterImpl extends Reporter {
  class Severity(val id: Int)(name: String) { var count: Int = 0 ; override def toString = name}
  object INFO    extends Severity(0)("INFO")
  object WARNING extends Severity(1)("WARNING")
  object ERROR   extends Severity(2)("ERROR")

  def count(severity: Severity): Int       = severity.count
  def resetCount(severity: Severity): Unit = severity.count = 0
}
