/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc;

object NoPhase extends Phase(null) {
  def name = "<no phase>";
  def run: unit = throw new Error("NoPhase.run");
}
