/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc

import scala.ref.WeakReference

/** An nsc sub-component.
 *
 *  @author Martin Odersky
 */
abstract class SubComponent {

  /** The global environment; overridden by instantiation in Global. */
  val global: Global

  /** The name of the phase */
  val phaseName: String

  /** List of phase names, this phase should run after  */
  val runsAfter: List[String]

  /** List of phase names, this phase should run before  */
  val runsBefore: List[String] = Nil

  /** Phase name this phase will attach itself to, not allowing any phase to come between it
   * and the phase name declared  */
  val runsRightAfter: Option[String]

  /** Internal flag to tell external from internal phases */
  val internal: Boolean = true

  /** SubComponent are added to a HashSet and two phases are the same if they have the same name  */
  override def hashCode() = phaseName.hashCode()

  /** New flags defined by the phase which are not valid before */
  def phaseNewFlags: Long = 0

  /** New flags defined by the phase which are not valid until immediately after it */
  def phaseNextFlags: Long = 0

  /** The phase factory */
  def newPhase(prev: Phase): Phase

  private var ownPhaseCache: WeakReference[Phase] = new WeakReference(null)
  private var ownPhaseRunId = global.NoRunId

  /** The phase corresponding to this subcomponent in the current compiler run */
  def ownPhase: Phase = {
    ownPhaseCache.get match {
      case Some(phase) if ownPhaseRunId == global.currentRunId =>
        phase
      case _ =>
        val phase = global.currentRun.phaseNamed(phaseName)
        ownPhaseCache = new WeakReference(phase)
        ownPhaseRunId = global.currentRunId
        phase
    }
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
