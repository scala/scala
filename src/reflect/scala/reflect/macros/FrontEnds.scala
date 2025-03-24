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

package scala
package reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.blackbox.Context the Scala macros context]] that
 *  provides facilities to communicate with the compiler's front end
 *  (emit warnings, errors and other sorts of messages).
 */
trait FrontEnds {
  self: blackbox.Context =>

  /** For sending a message which should not be labelled as a warning/error,
   *  but also shouldn't require -verbose to be visible.
   *  Use `enclosingPosition` if you're in doubt what position to pass to `pos`.
   */
  def echo(pos: Position, msg: String): Unit

  /** Emits an informational message, suppressed unless `-verbose` or `force=true`.
   *  Use `enclosingPosition` if you're in doubt what position to pass to `pos`.
   */
  def info(pos: Position, msg: String, force: Boolean): Unit

  /** Does the compilation session have any warnings?
   */
  def hasWarnings: Boolean

  /** Emits a warning.
   *  Use `enclosingPosition` if you're in doubt what position to pass to `pos`.
   */
  def warning(pos: Position, msg: String): Unit

  /** Does the compilation session have any errors?
   */
  def hasErrors: Boolean

  /** Emits a compilation error.
   *  Use `enclosingPosition` if you're in doubt what position to pass to `pos`.
   */
  def error(pos: Position, msg: String): Unit

  /** Abruptly terminates current macro expansion leaving a note about what happened.
   *  Use `enclosingPosition` if you're in doubt what position to pass to `pos`.
   */
  def abort(pos: Position, msg: String): Nothing
}
