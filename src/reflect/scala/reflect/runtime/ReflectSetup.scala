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

package scala
package reflect
package runtime

import internal.{SomePhase, NoPhase}

/** A helper trait to initialize things that need to be set before JavaMirrors and other
 *  reflect specific traits are initialized */
private[runtime] trait ReflectSetup extends internal.SymbolTable {
  Array(NoPhase, SomePhase).copyToArray(phaseWithId)
  override val currentRunId = 1 // fake a run id so that it is different from NoRunId
  phase = SomePhase // set to a phase different from NoPhase
}
