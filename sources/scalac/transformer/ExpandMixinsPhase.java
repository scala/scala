/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: ExpandMixinsPhase.java,v 1.8 2002/05/02 10:59:35 schinz Exp $
// $Id$

package scalac.transformer;

import scalac.*;
import scalac.checkers.*;

import java.util.*;

public class ExpandMixinsPhase extends PhaseDescriptor {
    /** Mapping from class symbols to class definitions */
    public Map/*<Symbol,Tree>*/ classDefs = new HashMap();

    public String name () {
        return "expandmixins";
    }

    public String description () {
        return "expand mixins by code copying";
    }

    public String taskDescription() {
        return "expanded mixins";
    }

    public void apply(Global global) {
        new ExpandMixins(global, this).apply();
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
