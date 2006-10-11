/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc

/** An nsc sub-component.
 *
 *  @author Martin Odersky
 */
abstract class SubComponent {

  /** The global environment; overridden by instantiation in Global. */
  val global: Global

  /** The name of the phase */
  val phaseName: String

  /** New flags defined by the phase which are not valid before */
  def phaseNewFlags: long = 0

  /** The phase factory */
  def newPhase(prev: Phase): Phase

  /** A standard phase template */
  abstract class StdPhase(prev: Phase) extends global.GlobalPhase(prev) {
    def name = phaseName
    override def newFlags = phaseNewFlags
  }
}
