/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac._;
import scalac.ast._;
import scalac.symtab._;
import scalac.checkers._;
import scalac.{Global => scalac_Global}

package scala.tools.scalac.typechecker {

import scalac.util.NewArray;

class RefCheckPhase(global: scalac_Global, descriptor: PhaseDescriptor)
  extends Phase(global, descriptor) {

    /** Applies this phase to the given compilation units. */
    def apply(units: Array[CompilationUnit]) = {
      var i = 0; while (i < units.length) {
        new RefCheck(global.asInstanceOf[scalac.Global]).apply(units(i));
        i = i + 1
      }
    }

    override def transformInfo(sym: Symbol, tp: Type): Type =
	if (sym.isModule() && !sym.isStatic()) new Type$PolyType(Symbol.EMPTY_ARRAY, tp);
	else tp;

    override def postCheckers(global: scalac_Global): Array[Checker] =
      NewArray.Checker(new CheckSymbols(global), new CheckTypes(global));
}
}
