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

  override def apply(unit: CompilationUnit): Unit = {
    global.start();
    unit.body = new Parser(unit).parse();
    global.stop("parsed " + unit.source);
  }
}
}
