/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import ch.epfl.lamp.util.Position;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.Unit;

/**
 * This class implements a phase that generates the HTML documentation
 * for Scala source code.
 */
public class HTMLGeneratorPhase extends Phase {

    //########################################################################
    // Public Constructors

    /**
     * Initializes this instance.
     *
     * @param global
     * @param descriptor
     */
    public HTMLGeneratorPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

    //########################################################################
    // Public Methods

    /**
     * Applies this phase to the given compilation units.
     *
     * @param units
     */
    public void apply(Unit[] units) {
        DocModule.apply(global);
    }

    //########################################################################
}
