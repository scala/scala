/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

package scalac.transformer;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.CompilationUnit;
import scalac.checkers.TreeChecker;
import scalac.symtab.Definitions;

import ch.epfl.lamp.util.CodePrinter;
import scalac.atree.ATreePrinter;


/**
 * This class represents the wholeprog phase for the java version
 * of the compiler. It doesn't do anything but permit to make
 * a bridge between the java implementation of Socos et the
 * scala one. See scala.tools.scalac.wholeprog.WholeProgPhase for
 * implementation
 */
public class WholeProgPhase extends Phase {

    /** Initializes this instance. */
    public WholeProgPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);

    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation units. */
    public void apply(CompilationUnit[] units) {
       	// This java version doesn't make anything
    }

}

