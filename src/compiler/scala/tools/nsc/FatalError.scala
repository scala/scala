/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

case class FatalError(msg: String) extends Exception(msg)
class MissingRequirementError(val req: String) extends FatalError(req + " not found.")
object MissingRequirementError {
  def unapply(x: Throwable) = x match {
    case x: MissingRequirementError => Some(x.req)
    case _                          => None
  }
}
