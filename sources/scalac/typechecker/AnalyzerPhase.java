/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.typechecker;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.CompilationUnit;
import scalac.ast.TreeGen;
import scalac.symtab.Definitions;
import scalac.symtab.Symbol;

public abstract class AnalyzerPhase extends Phase {

    public final TreeGen gen;

    /** Initializes this instance. */
    public AnalyzerPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
        global.definitions = new Definitions(global);
        this.gen = new TreeGen(global, global.make);
    }

    public abstract void addConsoleImport(Symbol module);

    public abstract CompilationUnit[] getUnits();

}
