/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */

package scalac.ast.parser;

import java.io.*;
import scalac.*;

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

    public Phase createPhase(Global global) {
	return new ParserWorker(global, this);
    }
}

public class ParserWorker extends Phase {

    /** constructor
     */
    public ParserWorker(Global global, PhaseDescriptor descr) {
        super(global, descr);
    }

    /** apply this phase to all compilation units
     */
    public void apply() {
        super.apply();
        int count = 0;
        for (int i = 0; i < global.units.length; i++) {
            if (global.units[i].body != null) count++;
        }
        Unit[] units = new Unit[count];
        for (int i = 0, j = 0; i < global.units.length; i++) {
            if (global.units[i].body != null) units[j++] = global.units[i];
        }
        global.units = units;
    }

    /** apply this phase to the given compilation unit
     */
    public void apply(Unit unit) {
        global.start();
        unit.body = new Parser(unit).parse();
        global.stop("parsed " + unit.source);
    }
}
