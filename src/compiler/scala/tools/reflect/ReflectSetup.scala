/* NSC -- new Scala compiler
 * Copyright 2012-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools
package reflect

import scala.tools.nsc.Global

/** A helper trait to initialize things that need to be set before JavaMirrors and other
 *  reflect specific traits are initialized */
private[reflect] trait ReflectSetup { this: Global =>
  phase = new Run().typerPhase
}