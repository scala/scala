/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.backend.msil;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.CompilationUnit;

import java.util.HashMap;

public class GenMSILPhase extends Phase {

    //########################################################################
    // Private Fields

    /** The tree to code translator */
    private final GenMSIL translator;

    final HashMap types2symbols = new HashMap();
    final HashMap symbols2types = new HashMap();
    final HashMap symbols2fields = new HashMap();
    final HashMap symbols2methods = new HashMap();
    final HashMap symbols2moduleFields = new HashMap();

    final TypeCreator tc;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public GenMSILPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
	this.tc = new TypeCreator(global, this);
        this.translator = new GenMSIL(global, this);
    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation units. */
    public void apply(CompilationUnit[] units) {
	tc.init();
	tc.collectSymbols(units);
	tc.initAssembly();
        for (int i = 0; i < units.length; i++) translator.apply(units[i]);
	tc.saveAssembly();
    }

    //########################################################################
}
