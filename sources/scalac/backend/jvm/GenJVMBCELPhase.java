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
 * Phase to generate Java bytecodes using the BCEL library.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class GenJVMBCELPhase extends Phase {

    //########################################################################
    // Private Fields

    /** The tree to code translator */
    private final GenJVMBCEL translator;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public GenJVMBCELPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
        this.translator = new GenJVMBCEL(global);
    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
        for (int i = 0; i < units.length; i++) translator.translate(units[i]);
    }

    //########################################################################
}
