/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.backend.msil;

import scalac.Global;
import scalac.Unit;
import scalac.Phase;
import scalac.PhaseDescriptor;

import java.util.HashMap;

public class GenMSILPhase extends PhaseDescriptor {

    final HashMap assemblies = new HashMap();

    final HashMap types2symbols = new HashMap();
    final HashMap symbols2types = new HashMap();
    final HashMap symbols2fields = new HashMap();
    final HashMap symbols2methods = new HashMap();
    final HashMap symbols2moduleFields = new HashMap();

    public String name () {
        return "genmsil";
    }

    public String description () {
        return "generate MSIL code";
    }

    public String taskDescription() {
        return "generated MSIL code";
    }

    public void apply(Global global) {
        new GenMSIL(global, this).apply();
    }

}
