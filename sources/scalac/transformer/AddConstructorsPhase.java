/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

//package scala.compiler.backend;
package scalac.transformer;

import java.util.HashMap;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.Unit;
import scalac.checkers.*;

public class AddConstructorsPhase extends Phase {

    //########################################################################
    // Private Fields

    /** A maps from old constructor symbols to new ones */
    private final HashMap/*<Symbol,Symbol>*/ constructors = new HashMap();

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public AddConstructorsPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
        for (int i = 0; i < units.length; i++)
            new AddConstructors(global, constructors).apply(units[i]);
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
