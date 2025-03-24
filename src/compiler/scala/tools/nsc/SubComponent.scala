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

package scala.tools.nsc

import scala.ref.WeakReference

/** Compilation is split into phases and the sub-components of the compiler define such phases.
 *
 *  @author Martin Odersky
 */
abstract class SubComponent {

  /** The global environment; overridden by instantiation in Global. */
  val global: Global

  /** The name of the phase */
  val phaseName: String

  /** Names of phases that must run before this phase. */
  val runsAfter: List[String]

  /** Names of phases that must run after this phase. Default is `Nil`. */
  val runsBefore: List[String] = Nil

  /** Name of the phase that this phase must follow immediately. */
  val runsRightAfter: Option[String]

  /** Names of phases required by this component. Default is `Nil`. */
  val requires: List[String] = Nil

  /** Is this component enabled? Default is true. */
  def enabled: Boolean = true

  /** True if this phase is not provided by a plug-in. */
  val internal: Boolean = true

  /** True if this phase runs before all other phases. Usually, `parser`. */
  val initial: Boolean = false

  /** True if this phase runs after all other phases. Usually, `terminal`. */
  val terminal: Boolean = false

  /** SubComponents are added to a HashSet and two phases are the same if they have the same name. */
  override def hashCode() = phaseName.hashCode()

  override def equals(other: Any) = other match {
    case other: SubComponent => phaseName.equals(other.phaseName)
    case _ => false
  }

  /** New flags defined by the phase which are not valid before */
  def phaseNewFlags: Long = 0

  /** New flags defined by the phase which are not valid until immediately after it */
  def phaseNextFlags: Long = 0

  /** The phase factory */
  def newPhase(prev: Phase): Phase

  private var ownPhaseCache: WeakReference[Phase] = new WeakReference(null)
  private var ownPhaseRunId = global.NoRunId

  @inline final def beforeOwnPhase[T](op: => T) = global.enteringPhase(ownPhase)(op)
  @inline final def afterOwnPhase[T](op: => T)  = global.exitingPhase(ownPhase)(op)

  /** The phase corresponding to this subcomponent in the current compiler run. */
  def ownPhase: Phase = {
    val cache = ownPhaseCache.underlying.get
    if (cache != null && ownPhaseRunId == global.currentRunId)
      cache
    else {
        val phase = global.currentRun.phaseNamed(phaseName)
        ownPhaseCache = new WeakReference(phase)
        ownPhaseRunId = global.currentRunId
        phase
    }
  }

  /** A standard phase template */
  abstract class StdPhase(prev: Phase) extends global.GlobalPhase(prev) {
    def name = phaseName
    override def newFlags = phaseNewFlags
    override def nextFlags = phaseNextFlags
  }
}
