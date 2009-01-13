/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
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
  def phaseNewFlags: Long = 0

  /** New flags defined by the phase which are not valid until immediately after it */
  def phaseNextFlags: Long = 0

  /** The phase factory */
  def newPhase(prev: Phase): Phase

  private var ownPhaseCache: Phase = _
  private var ownPhaseRunId = global.NoRunId

  /** The phase corresponding to this subcomponent in the current compiler run */
  def ownPhase: Phase = {
    if (ownPhaseRunId != global.currentRunId) {
      ownPhaseCache = global.currentRun.phaseNamed(phaseName)
      ownPhaseRunId = global.currentRunId
    }
    ownPhaseCache
  }

  /** The phase defined by this subcomponent. Can be called only after phase is installed by newPhase. */
//  lazy val ownPhase: Phase = global.currentRun.phaseNamed(phaseName)

  /** A standard phase template */
  abstract class StdPhase(prev: Phase) extends global.GlobalPhase(prev) {
    def name = phaseName
    override def newFlags = phaseNewFlags
    override def nextFlags = phaseNextFlags
  }
}
