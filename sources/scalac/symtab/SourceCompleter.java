/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab;

import ch.epfl.lamp.util.SourceFile;

import scalac.*;
import scalac.ast.parser.*;
import scalac.typechecker.Analyzer;
import java.io.*;


public class SourceCompleter extends Type.LazyType {

    /** the global compilation environment
     */
    protected Global global;
    protected String filename;
    private boolean completed = false;

    public SourceCompleter(Global global, String filename) {
        this.global = global;
        this.filename = filename;
    }

    /** complete class symbol c by loading the class
     */
    public void complete(Symbol c) {
        if (completed) {
            c.setInfo(Type.NoType);
        } else if (filename != null) {
            try {
                String fname = filename;
                long msec = System.currentTimeMillis();
                Unit unit = new Unit(global, new SourceFile(filename), false);
                filename = null;
                global.PHASE.PARSER.apply(unit);
                global.PHASE.ANALYZER.lateEnter(global, unit, c);
                global.operation("added " + fname + " in " +
                        (System.currentTimeMillis() - msec) + "ms");
            } catch (IOException e) {
                e.printStackTrace();
                global.error("i/o error while loading " + c);
                c.setInfo(Type.ErrorType);
            }
            completed = true;
        }
    }
}
