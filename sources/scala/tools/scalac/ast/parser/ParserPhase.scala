/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scalac.ast.parser;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.Unit;

class ParserPhase(global: Global, descriptor: PhaseDescriptor) extends Phase(global, descriptor) {

  def apply(units: Array[Unit]): unit = {
    for (val i <- Iterator.range(0, units.length)) do {
      global.start();
      units(i).body = new Parser(units(i)).parse();
      global.stop("parsed " + units(i).source);
    }
  }
}
