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

package scala.tools
package reflect

import scala.tools.nsc.Global

/** A helper trait to initialize things that need to be set before JavaMirrors and other
 *  reflect specific traits are initialized */
private[reflect] trait ReflectSetup { this: Global =>
  phase = new Run().typerPhase
}
