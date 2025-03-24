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

package scala.tools.nsc
package reporters

import scala.annotation.{nowarn, unused}
import scala.collection.mutable
import scala.reflect.internal
import scala.reflect.internal.util.{Position, ScalaClassLoader}

/** This class exists for sbt compatibility. Global.reporter holds a FilteringReporter.
  * The only Reporter that is *not* a FilteringReporter is the one created by sbt.
  * The Global.reporter_= setter wraps that in a delegating [[MakeFilteringForwardingReporter]].
  */
abstract class Reporter extends internal.Reporter {
  // used by sbt
  @deprecated("Use echo, as internal.Reporter does not support unforced info", since="2.13.0")
  final def info(pos: Position, msg: String, @unused force: Boolean): Unit = info0(pos, msg, INFO, force = true)

  // overridden by sbt, IDE -- should not be in the reporting interface
  // (IDE receives comments from ScaladocAnalyzer using this hook method)
  // TODO: IDE should override a hook method in the parser instead
  def comment(pos: Position, msg: String): Unit = ()

  // used by sbt (via unit.cancel) to cancel a compile (see hasErrors)
  // TODO: figure out how sbt uses this, come up with a separate interface for controlling the build
  private[this] var _cancelled: Boolean = false
  def cancelled: Boolean = _cancelled
  def cancelled_=(b: Boolean): Unit = _cancelled = b

  override def hasErrors: Boolean = super.hasErrors || cancelled

  override def reset(): Unit = {
    super.reset()
    cancelled = false
  }
}

object Reporter {
  /** The usual way to create the configured reporter.
   *  Errors are reported through `settings.errorFn` and also by throwing an exception.
   */
  def apply(settings: Settings): FilteringReporter = {
    //val loader = ScalaClassLoader(getClass.getClassLoader)  // apply does not make delegate
    val loader = new ClassLoader(getClass.getClassLoader) with ScalaClassLoader
    loader.create[FilteringReporter](settings.reporter.value, settings.errorFn)(settings)
  }
}

/** The reporter used in a Global instance.
  *
  * It filters messages based on
  *   - settings.maxerrs / settings.maxwarns
  *   - positions (only one error at a position, no duplicate messages on a position)
  */
abstract class FilteringReporter extends Reporter {
  def settings: Settings

  @deprecatedOverriding("override the `doReport` overload (defined in reflect.internal.Reporter) instead", "2.13.12")
  @deprecated("use the `doReport` overload instead", "2.13.12")
  def doReport(pos: Position, msg: String, severity: Severity): Unit = doReport(pos, msg, severity, Nil)

  // this should be the abstract method all the way up in reflect.internal.Reporter, but sbt compat
  // the abstract override is commented-out to maintain binary compatibility for FilteringReporter subclasses
  // override def doReport(pos: Position, msg: String, severity: Severity, actions: List[CodeAction]): Unit

  @deprecatedOverriding("override `doReport` instead", "2.13.1") // overridden in scalameta for example
  @nowarn("cat=deprecation")
  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit =
    // call the deprecated overload to support existing FilteringReporter subclasses (they override that overload)
    doReport(pos, msg, severity)

  private lazy val positions = mutable.Map[Position, Severity]() withDefaultValue INFO
  private lazy val messages  = mutable.Map[Position, List[String]]() withDefaultValue Nil

  private def maxErrors: Int = settings.maxerrs.value
  private def maxWarnings: Int = settings.maxwarns.value

  override def filter(pos: Position, msg: String, severity: Severity): Int = {
    import internal.Reporter.{ERROR => Error, WARNING => Warning, _}
    def maxOk = severity match {
      case Error   => maxErrors < 0   || errorCount < maxErrors
      case Warning => maxWarnings < 0 || warningCount < maxWarnings
      case _       => true
    }
    // Invoked when an error or warning is filtered by position.
    @inline def suppress = {
      if (settings.prompt.value) doReport(pos, msg, severity, Nil)
      else if (settings.isDebug) doReport(pos, s"[ suppressed ] $msg", severity, Nil)
      Suppress
    }
    if (!duplicateOk(pos, severity, msg)) suppress else if (!maxOk) Count else Display
  }

  /** Returns `true` if the message should be reported. Messages are skipped if:
    *   - there was already some error at the position. After an error, no further
    *     messages at that position are issued.
    *   - the same warning/info message was already issued at the same position.
    * Note: two positions are considered identical for logging if they have the same point.
    */
  private def duplicateOk(pos: Position, severity: Severity, msg: String): Boolean = {
    // was a prefix of the msg already reported at this position for purposes of suppressing repetition?
    def matchAt(pos: Position, msg: String): Boolean = messages(pos).exists(msg.startsWith)

    // always report at null / NoPosition
    pos == null || !pos.isDefined || {
      val fpos = pos.focus
      val show = positions(fpos) match {
        case internal.Reporter.ERROR => false               // already error at position
        case s if s.id > severity.id => false               // already message higher than present severity
        case `severity`              => !matchAt(fpos, msg) // already issued this (in)exact message
        case _                       => true                // good to go
      }
      if (show) {
        positions(fpos) = severity
        messages(fpos) ::= stripQuickfixable(msg)
      }
      show
    }
  }

  private def stripQuickfixable(msg: String): String = {
    val i = msg.indexOf(" [quickfixable]")
    if (i > 0) msg.substring(0, i) else msg
  }

  override def reset(): Unit = {
    super.reset()
    positions.clear()
    messages.clear()
  }
}
