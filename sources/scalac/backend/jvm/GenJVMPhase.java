/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $OldId: GenJVMPhase.java,v 1.1 2002/09/03 12:13:08 schinz Exp $
// $Id$

package scalac.backend.jvm;

import scalac.Global;
import scalac.Unit;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.ApplicationError;


public class GenJVMPhase extends PhaseDescriptor {

    public String name () {
        return "genjvm";
    }

    public String description () {
        return "generate JVM bytecodes";
    }

    public String taskDescription() {
        return "generated JVM code";
    }

    public Phase createPhase(Global global) {
        return new GenJVM(global, this);
    }

}
