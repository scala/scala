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

/**
 * This class represents the wholeprog phase for the java version of
 * the compiler. It doesn't do anything but permit to make a bridge
 * between the java implementation of Socos and the scala one. See
 * scala.tools.scalac.wholeprog.WholeProgPhase for implementation.
 */
public abstract class WholeProgPhase extends Phase {

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public WholeProgPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

    //########################################################################
}
