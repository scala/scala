/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$
// $OldId: ExplicitOuterClassesPhase.java,v 1.8 2002/08/21 14:08:18 paltherr Exp $

package scalac.transformer;

import scalac.*;

public class ExplicitOuterClassesPhase extends PhaseDescriptor {
    public String name () {
        return "explicitouterclasses";
    }

    public String description () {
        return "make links from inner classes to enclosing one explicit";
    }

    public String taskDescription() {
        return "made outer links explicit";
    }

    public Phase createPhase(Global global) {
        return new ExplicitOuterClasses(global, this);
    }

//     public Checker[] postCheckers(Global global) {
//         return new Checker[] {
//             new CheckSymbols(global),
//             new CheckTypes(global),
//             new CheckOwners(global)
//         };
//     }
}
