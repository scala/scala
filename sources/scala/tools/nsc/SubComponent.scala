/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$
package scala.tools.nsc;

/** An nsc sub-component.
 */
abstract class SubComponent {
  val global: Global;
  val phaseName: String;
  def newPhase(prev: Phase): Phase;
  abstract class StdPhase(prev: Phase) extends global.GlobalPhase(prev) {
    def name = phaseName;
  }
}
