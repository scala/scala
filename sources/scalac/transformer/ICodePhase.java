/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import ch.epfl.lamp.util.CodePrinter;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.CompilationUnit;

/**
 * This class represents the ICode phase for the java version
 * of the compiler. It doesn't do anything but permit to make
 * a bridge between the java implementation of Socos et the
 * scala one. See scala.tools.scalac.icode.ICodePhase for
 * implementation
 */
public abstract class ICodePhase extends Phase {

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public ICodePhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

    //########################################################################
    // Public Methods

    /** Prints the given compilation units. */
    public abstract void print(CompilationUnit[] units, CodePrinter printer);

    //########################################################################
}
