/* NSC -- new Scala compiler
 * Copyright 2007-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

object NoPhase extends Phase(null) {
  def name = "<no phase>"
  def run { throw new Error("NoPhase.run") }
}
