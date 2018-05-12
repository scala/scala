/* NSC -- new Scala compiler
 * Copyright 2002-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package reporters

import scala.reflect.internal.util._

/** Report information, warnings and errors.
 *
 *  This describes the internal interface for issuing information, warnings and errors.
 *  The only abstract method in this class must be info0.
 *
 *  TODO: Move external clients (sbt/ide) to reflect.internal.Reporter, and remove this class.
 */
@deprecated("Use reflect.internal.Reporter", since="2.13.0")
abstract class Reporter extends scala.reflect.internal.Reporter {
  /** Informational messages. If `!force`, they may be suppressed. */
  @deprecated("Use echo, as internal.Reporter does not support unforced info", since="2.13.0")
  final def info(pos: Position, msg: String, force: Boolean): Unit = info0(pos, msg, INFO, force)

  /* For sending a message which should not be labelled as a warning/error,
   * but also shouldn't require -verbose to be visible.
  def echo(msg: String): Unit = info(NoPosition, msg, force = true)
   */

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
