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

package scala.tools.scalac.ast.parser {

class ParserPhase(global: scalac_Global, descriptor: PhaseDescriptor) extends Phase(global, descriptor) {

  def apply(units: Array[CompilationUnit]): unit = {
    for (val i <- Iterator.range(0, units.length)) {
      global.start();
      units(i).body = new Parser(units(i)).parse();
      global.stop("parsed " + units(i).source);
    }
  }
}
}
