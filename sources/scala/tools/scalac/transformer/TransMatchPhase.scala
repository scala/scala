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
import scalac.checkers._;

package scala.tools.scalac.transformer {

class TransMatchPhase(global:scalac_Global, descriptor:PhaseDescriptor )
  extends Phase(global, descriptor) {

    /** Applies this phase to the given compilation units. */
    def apply( units:Array[CompilationUnit] ):unit = {
      for(val u <- units) {
        new TransMatch( global ).apply( u );
      }
    }
    override def postCheckers( global:scalac_Global ):Array[Checker] = {
      val a = new Array[Checker](2);
      a.update(0, new CheckSymbols(global) );
      a.update(1, new CheckTypes(global) );
      a
    }
  }
}


