/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.CompilationUnit;
import scalac.checkers.TreeChecker;
import scalac.symtab.Definitions;

/**
 * This phase makes boxing and unboxing of primitive values and arrays
 * explicit.
 */
public class MakeBoxingExplicitPhase extends Phase {

    //########################################################################
    // Private Fields

    private final Definitions definitions;
    private final TreeChecker checker;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public MakeBoxingExplicitPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
        this.definitions = global.definitions;
        this.checker = new TreeChecker(definitions);
    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation units. */
    public void apply(CompilationUnit[] units) {
        for (int i = 0; i < units.length; i++) {
            assert checker.check(units[i]);
            new scalac.atree.ATreeFromSTree(global.definitions)
                .translate(units[i]); // !!!
        }
    }

    //########################################################################
}
