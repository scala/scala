/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.util;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.CompilationUnit;

/** This class implements a phase that does nothing. */
public class EmptyPhase extends Phase {

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public EmptyPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation units. */
    public void apply(CompilationUnit[] units) {
        // do nothing
    }

    //########################################################################
}
