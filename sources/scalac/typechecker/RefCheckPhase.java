/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.typechecker;

import scalac.*;
import scalac.ast.*;
import scalac.checkers.*;

public class RefCheckPhase extends Phase {

    /** Initializes this instance. */
    public RefCheckPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
        for (int i = 0; i < units.length; i++)
            new RefCheck(global).apply(units[i]);
    }

    public Checker[] postCheckers(Global global) {
        return new Checker[] {
            new CheckSymbols(global),
            new CheckTypes(global),
            new CheckOwners(global),
            new CheckNames(global)
        };
    }
}
