/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.{Global => scalac_Global}
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.CompilationUnit;

package scala.tools.scalac.transformer {

class TransMatchPhase(global:scalac_Global, descriptor:PhaseDescriptor )
  extends Phase(global, descriptor) {

    /** Applies this phase to the given compilation unit. */
    override def apply(unit: CompilationUnit): Unit =
      new TransMatch(global).apply(unit);

  }
}


