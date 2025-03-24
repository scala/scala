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

package scala.reflect.macros
package contexts

import scala.reflect.macros.runtime.AbortMacroException
import scala.tools.nsc.Reporting.WarningCategory

trait FrontEnds {
  self: Context =>

  def echo(pos: Position, msg: String): Unit = universe.reporter.echo(pos, msg)

  @deprecated("Use echo, info messages are always forced", since="2.13.0")
  def info(pos: Position, msg: String, force: Boolean): Unit = universe.reporter.echo(pos, msg)

  def hasWarnings: Boolean = universe.reporter.hasErrors

  def hasErrors: Boolean = universe.reporter.hasErrors

  // TODO: add WarningCategory parameter (not binary compatible)
  def warning(pos: Position, msg: String): Unit = callsiteTyper.context.warning(pos, msg, WarningCategory.Other)

  def error(pos: Position, msg: String): Unit = callsiteTyper.context.error(pos, msg)

  def abort(pos: Position, msg: String): Nothing = throw new AbortMacroException(pos, msg)
}
