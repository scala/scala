/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: AddAccessorsPhase.java,v 1.1 2002/10/17 12:27:11 schinz Exp $
// $Id$

package scalac.transformer;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.Unit;
import scalac.checkers.*;

public class AddAccessorsPhase extends Phase {

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public AddAccessorsPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
        for (int i = 0; i < units.length; i++)
            new AddAccessors(global).apply(units[i]);
    }

    public Checker[] postCheckers(Global global) {
        return new Checker[] {
            new CheckSymbols(global),
            new CheckTypes(global),
            new CheckOwners(global),
	    new CheckNames(global)
        };
    }

    //########################################################################
}
