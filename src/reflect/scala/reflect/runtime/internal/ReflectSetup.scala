package scala.reflect.runtime.internal

import scala.reflect.internal.{SomePhase, NoPhase, Phase, TreeGen}

/** A helper trait to initialize things that need to be set before JavaMirrors and other
 *  reflect specific traits are initialized */
private[runtime] trait ReflectSetup extends scala.reflect.internal.SymbolTable {
  override val phaseWithId: Array[Phase] = Array(NoPhase, SomePhase)
  override val currentRunId = 1 // fake a run id so that it is different from NoRunId
  phase = SomePhase // set to a phase different from NoPhase
}
