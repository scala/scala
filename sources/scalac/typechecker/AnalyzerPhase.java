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
import scalac.Unit;
import scalac.symtab.Symbol;

public abstract class AnalyzerPhase extends Phase {

    /** Initializes this instance. */
    public AnalyzerPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

    public abstract void addConsoleImport(Symbol module);

    public abstract void lateEnter(Global global, Unit unit, Symbol symbol);

}
