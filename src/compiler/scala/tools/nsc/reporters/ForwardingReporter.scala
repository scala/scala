/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.reporters
import scala.reflect.internal.settings.MutableSettings
import scala.reflect.internal.util.{CodeAction, Position}
import scala.tools.nsc.Settings


/** A FilteringReporter that delegates to another FilteringReporter. This class can be used to
  * customize error reporting.
  * {{{
  *   val myReporter = new ForwardingReporter(global.reporter) {
  *     override def doReport(pos: Position, msg: String, severity: Severity, actions: List[Action]): Unit = { ... }
  *   }
  *   global.reporter = myReporter
  * }}}
  */
class ForwardingReporter(delegate: FilteringReporter) extends FilteringReporter {
  def settings: Settings = delegate.settings

  override def doReport(pos: Position, msg: String, severity: Severity, actions: List[CodeAction]): Unit =
    delegate.doReport(pos, msg, severity, actions)

  override def filter(pos: Position, msg: String, severity: Severity): Int = delegate.filter(pos, msg, severity)

  override def increment(severity: Severity): Unit = delegate.increment(severity)

  override def errorCount: Int = delegate.errorCount
  override def warningCount: Int = delegate.warningCount

  override def hasErrors: Boolean = delegate.hasErrors
  override def hasWarnings: Boolean = delegate.hasWarnings

  override def comment(pos: Position, msg: String): Unit = delegate.comment(pos, msg)

  override def cancelled: Boolean = delegate.cancelled
  override def cancelled_=(b: Boolean): Unit = delegate.cancelled_=(b)

  override def flush(): Unit = delegate.flush()
  override def finish(): Unit = delegate.finish()
  override def reset(): Unit = delegate.reset() // super.reset not necessary, own state is never modified

  override def rerunWithDetails(setting: MutableSettings#Setting, name: String): String = delegate.rerunWithDetails(setting, name)
}

/** Used in `Global.reporter_=`. sbt assigns a plain `Reporter`, which is then adapted to respect
  * maxerrs and do position filtering.
  */
class MakeFilteringForwardingReporter(delegate: Reporter, val settings: Settings) extends FilteringReporter {
  override def doReport(pos: Position, msg: String, severity: Severity, actions: List[CodeAction]): Unit =
    delegate.doReport(pos, msg, severity, actions)

  override def increment(severity: Severity): Unit = delegate.increment(severity)

  override def errorCount: Int = delegate.errorCount
  override def warningCount: Int = delegate.warningCount

  override def hasErrors: Boolean = delegate.hasErrors
  override def hasWarnings: Boolean = delegate.hasWarnings

  override def comment(pos: Position, msg: String): Unit = delegate.comment(pos, msg)

  override def cancelled: Boolean = delegate.cancelled
  override def cancelled_=(b: Boolean): Unit = delegate.cancelled_=(b)

  override def flush(): Unit = delegate.flush()
  override def finish(): Unit = delegate.finish()
  override def reset(): Unit = {
    super.reset() // reset own filtering state
    delegate.reset()
  }

  override def rerunWithDetails(setting: MutableSettings#Setting, name: String): String = delegate.rerunWithDetails(setting, name)
}
