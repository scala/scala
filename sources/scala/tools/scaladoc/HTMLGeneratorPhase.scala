/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.CompilationUnit;

package scala.tools.scaladoc {

class HTMLGeneratorPhase(global: Global, descriptor: PhaseDescriptor)
  extends Phase(global, descriptor)
{

  def apply(units: Array[CompilationUnit]): Unit =
    (new HTMLGenerator(global) {
      def newTypeIso(global: Global): TypeIsomorphism = new ScalaML(global);
    }).apply();

}

}
