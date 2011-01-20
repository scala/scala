/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import scala.util.control.ControlThrowable

case class FatalError(msg: String) extends Throwable(msg)

class MissingRequirementError(val req: String) extends FatalError(req + " not found.")

object MissingRequirementError {
  def unapply(x: Throwable) = x match {
    case x: MissingRequirementError => Some(x.req)
    case _                          => None
  }
}
