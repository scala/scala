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
import scalac.typechecker.AnalyzerPhase;
import java.io.*;


public class SourceCompleter extends Type.LazyType {

    /** the global compilation environment
     */
    protected final Global global;
    private final boolean mixinOnly;

    private boolean completed = false;

    public SourceCompleter(Global global) {
        this(global, false);
    }

    public SourceCompleter(Global global, boolean mixinOnly) {
        this.global = global;
	this.mixinOnly = mixinOnly;
    }

    /** complete class symbol c by loading the unit
     */
    public void complete(Symbol c) {
        if (completed) {
            c.setInfo(Type.NoType);
        } else {
            try {
                long msec = System.currentTimeMillis();
		String filename = SourceRepresentation.externalizeFileName(
		    c.fullName()) + ".scala";
		java.io.File f = global.classPath.openJavaFile(filename);
                Unit unit = new Unit(global, new SourceFile(f), false, mixinOnly);
                Phase phase = global.currentPhase;
                global.currentPhase = global.PHASE.PARSER.phase();
                global.PHASE.PARSER.phase().apply(new Unit[] {unit});
                global.currentPhase = global.PHASE.ANALYZER.phase();
                ((AnalyzerPhase)global.PHASE.ANALYZER.phase()).lateEnter(global, unit, c);
                global.currentPhase = phase;
                global.operation("added " + filename + " in " +
                        (System.currentTimeMillis() - msec) + "ms");
            } catch (IOException e) {
                if (global.debug) e.printStackTrace();
		if (mixinOnly)
		    global.error("source file for " + c + " not found; it is needed because class is used as a mixin");
		else
		    global.error("i/o error while loading " + c + ": " + e);
                c.setInfo(Type.ErrorType);
            }
            completed = true;
        }
    }
}
