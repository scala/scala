/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id:

package scalac.transformer;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.Unit;
import scalac.checkers.TreeChecker;
import scalac.symtab.Definitions;

import ch.epfl.lamp.util.CodePrinter;
import scalac.atree.ATreePrinter;


/**
 * This class represents the ICode phase for the java version
 * of the compiler. It doesn't do anything but permit to make
 * a bridge between the java implementation of Socos et the
 * scala one. See scala.tools.scalac.icode.ICodePhase for
 * implementation
 */
public class ICodePhase extends Phase {

    //########################################################################
    // Private Fields

    private final Definitions definitions;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public ICodePhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
        this.definitions = global.definitions;
    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
       	// This java version doesn't make anything
    }

    public ATreePrinter getPrinter(CodePrinter cp) {
	return new ATreePrinter(cp);
	// !! Useless
    }

}

