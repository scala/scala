/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import scalac.*;
import scalac.checkers.*;

public class TransMatchPhase extends PhaseDescriptor {

    public String name () {
        return "transmatch";
    }

    public String description () {
        return "translate match expressions";
    }

    public String taskDescription() {
        return "translated pattern matching";
    }

    public void apply(Global global) {
        new TransMatch(global, this).apply();
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
