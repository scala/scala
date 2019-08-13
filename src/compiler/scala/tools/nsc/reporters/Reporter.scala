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
package reporters

import scala.reflect.internal.{ForwardingReporter, Reporter => InternalReporter}
import scala.reflect.internal.util.{Position, ScalaClassLoader}

/** Report information, warnings and errors.
 *
 *  This describes the internal interface for issuing information, warnings and errors.
 *  The only abstract method in this class must be info0.
 *
 *  TODO: Move external clients (sbt/ide) to reflect.internal.Reporter, and remove this class.
 */
@deprecated("Use reflect.internal.Reporter", since="2.13.0")
abstract class Reporter extends InternalReporter {
  /** Informational messages. If `!force`, they may be suppressed. */
  @deprecated("Use echo, as internal.Reporter does not support unforced info", since="2.13.0")
  final def info(pos: Position, msg: String, force: Boolean): Unit = info0(pos, msg, INFO, force)

  // overridden by sbt, IDE -- should not be in the reporting interface
  // (IDE receives comments from ScaladocAnalyzer using this hook method)
  // TODO: IDE should override a hook method in the parser instead
  def comment(pos: Position, msg: String): Unit = {}

  // used by sbt (via unit.cancel) to cancel a compile (see hasErrors)
  // TODO: figure out how sbt uses this, come up with a separate interface for controlling the build
  var cancelled: Boolean = false

  override def hasErrors: Boolean = super.hasErrors || cancelled

  override def reset(): Unit = {
    super.reset()
    cancelled = false
  }
}

object Reporter {
  /** Adapt a reporter to legacy reporter API. Handle `info` by forwarding to `echo`. */
  class AdaptedReporter(val delegate: InternalReporter) extends Reporter with ForwardingReporter {
    override protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = delegate.echo(pos, msg)

    override def count(severity: Severity): Unit = delegate.count(severity)

    override def errorCount   = delegate.errorCount
    override def warningCount = delegate.warningCount
    override def hasErrors    = delegate.hasErrors
    override def hasWarnings  = delegate.hasWarnings

    override def toString() = s"AdaptedReporter($delegate)"
  }
  /** A marker trait for adapted reporters that respect maxerrs. */
  trait LimitedReporter { _: Reporter => }
  /** A legacy `Reporter` adapter that respects `-Xmaxerrs` and `-Xmaxwarns`.  */
  class LimitingReporter(settings: Settings, delegate0: Reporter) extends AdaptedReporter(delegate0) with FilteringReporter with LimitedReporter {
    override protected def filter(pos: Position, msg: String, severity: Severity) =
      severity match {
        case InternalReporter.ERROR   => errorCount   < settings.maxerrs.value
        case InternalReporter.WARNING => warningCount < settings.maxwarns.value
        case _       => true
      }
    // work around fractured API to support `reporters.Reporter.info`, which is not forwarded
    override protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit =
      severity match {
        case InternalReporter.ERROR      => delegate.error(pos, msg)     // for symmetry, but error and warn are already forwarded
        case InternalReporter.WARNING    => delegate.warning(pos, msg)
        case _ if force => delegate.echo(pos, msg)
        case _          => delegate.info(pos, msg, force = false)
      }
  }
  // mark reporters known to respect maxerrs
  implicit def `adapt reporter to legacy API`(reporter: InternalReporter): Reporter =
    reporter match {
      case _: LimitFilter => new AdaptedReporter(reporter) with LimitedReporter
      case _              => new AdaptedReporter(reporter)
    }
  // whitelist reporters known to respect maxerrs; otherwise, enforce user-specified reduced limit
  def limitedReporter(settings: Settings, reporter: Reporter): Reporter =
    reporter match {
      case _: ConsoleReporter | _: LimitedReporter => reporter
      case _ if settings.maxerrs.isSetByUser && settings.maxerrs.value < settings.maxerrs.default =>
        new LimitingReporter(settings, reporter)
      case _ => reporter
    }

  /** The usual way to create the configured reporter.
   *  Errors are reported through `settings.errorFn` and also by throwing an exception.
   */
  def apply(settings: Settings): Reporter = {
    //val loader = ScalaClassLoader(getClass.getClassLoader)  // apply does not make delegate
    val loader = new ClassLoader(getClass.getClassLoader) with ScalaClassLoader
    val res = loader.create[InternalReporter](settings.reporter.value, settings.errorFn)(settings)
    if (res.isInstanceOf[Reporter]) res.asInstanceOf[Reporter]
    else res: Reporter  // adaptable
  }
}
