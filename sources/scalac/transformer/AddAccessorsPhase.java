/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: AddAccessorsPhase.java,v 1.1 2002/10/17 12:27:11 schinz Exp $
// $Id$

package scalac.transformer;

import scalac.*;
import scalac.checkers.*;

import java.util.*;

public class AddAccessorsPhase extends PhaseDescriptor {
    public String name () {
        return "addaccessors";
    }

    public String description () {
        return "add accessors for constructor arguments";
    }

    public String taskDescription() {
        return "added accessors";
    }

    public void apply(Global global) {
        new AddAccessors(global, this).apply();
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
