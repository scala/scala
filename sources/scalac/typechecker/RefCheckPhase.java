/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */

package scalac.typechecker;

import scalac.*;
import scalac.ast.*;
import scalac.checkers.*;

public class RefCheckPhase extends PhaseDescriptor {

    public String name() {
        return "refcheck";
    }

    public String description () {
        return "reference checking";
    }

    public String taskDescription () {
        return "reference checking";
    }

    public Phase createPhase(Global global) {
	return new RefCheck(global, this);
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
