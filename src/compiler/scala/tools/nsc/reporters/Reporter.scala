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
import scala.annotation.unused

/** This class exists for sbt compatibility. Global.reporter holds a FilteringReporter.
 *  The only Reporter that is *not* a FilteringReporter is the one created by sbt.
 *  The Global.reporter_= setter wraps that in a delegating [[MakeFilteringForwardingReporter]].
 */
abstract class Reporter extends InternalReporter {
  // used by sbt
  final def info(pos: Position, msg: String, @unused force: Boolean): Unit = info0(pos, msg, INFO, force = true)

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
  /** The usual way to create the configured reporter.
   *  Errors are reported through `settings.errorFn` and also by throwing an exception.
   */
  def apply(settings: Settings): FilteringReporter = {
    //val loader = ScalaClassLoader(getClass.getClassLoader)  // apply does not make delegate
    val loader = new ClassLoader(getClass.getClassLoader) with ScalaClassLoader
    loader.create[FilteringReporter](settings.reporter.value, settings.errorFn)(settings)
  }

  /** Take the message with its explanation, if it has one. */
  def explanation(msg: String): String = ???

}

/** The reporter used in a Global instance.
 *
 *  It filters messages based on
 *    - settings.nowarn
 *    - settings.maxerrs / settings.maxwarns
 *    - positions (only one error at a position, no duplicate messages on a position)
 */
abstract class FilteringReporter extends Reporter {

  // should be a ctor param
  def settings: Settings

  // this should be the abstract method all the way up in reflect.internal.Reporter, but sbt compat
  def doReport(pos: Position, msg: String, severity: Severity): Unit

  final protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = doReport(pos, msg, severity)
}

/** Used in `Global.reporter_=`. sbt assigns a plain `Reporter`, which is then adapted to respect
 *  maxerrs and do position filtering.
 */
final class MakeFilteringForwardingReporter(delegate: Reporter, val settings: Settings) extends FilteringReporter {
  def doReport(pos: Position, msg: String, severity: Severity): Unit = ???
}
