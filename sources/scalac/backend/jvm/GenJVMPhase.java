/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.backend.jvm;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.Unit;

/**
 * Phase to generate Java bytecodes using the FJBG library.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class GenJVMPhase extends Phase {

    //########################################################################
    // Private Fields

    /** The tree to code translator */
    private final GenJVM translator;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public GenJVMPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
        this.translator = new GenJVM(global);
    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
        for (int i = 0; i < units.length; i++) translator.translate(units[i]);
    }

    //########################################################################
}
