/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.ast.parser;

import scalac.Global;
import scalac.Unit;
import scalac.PhaseDescriptor;

public class ParserPhase extends PhaseDescriptor {

    public String name() {
        return "parse";
    }

    public String description () {
        return "parse source files";
    }

    public String taskDescription() {
        return "parsed";
    }

    public void apply(Global global) {
        for (int i = 0; i < global.units.length; i++) apply(global.units[i]);
    }

    public void apply(Unit unit) {
        unit.global.start();
        unit.body = new Parser(unit).parse();
        unit.global.stop("parsed " + unit.source);
    }

}
