/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.backend.jvm;

import scalac.Global;
import scalac.Unit;
import scalac.PhaseDescriptor;
import scalac.ApplicationError;


public class GenJVMBCELPhase extends PhaseDescriptor {

    public String name () {
        return "genjvm-bcel";
    }

    public String description () {
        return "generate JVM bytecodes";
    }

    public String taskDescription() {
        return "generated JVM code";
    }

    public void apply(Global global) {
        for (int i = 0; i < global.units.length; i++)
            apply(global.units[i]);
    }

    public void apply(Unit unit) {
        new GenJVMBCEL(unit.global).translate(unit);
    }
}
