/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab;

import scala.tools.util.AbstractFile;
import scala.tools.util.SourceFile;

import scalac.*;
import scalac.ast.parser.*;
import scalac.typechecker.AnalyzerPhase;
import scalac.util.SourceRepresentation;
import java.io.*;


public class SourceCompleter extends SymbolLoader {

    private final boolean mixinOnly;

    public SourceCompleter(Global global) {
        this(global, false);
    }

    public SourceCompleter(Global global, boolean mixinOnly) {
        super(global);
	this.mixinOnly = mixinOnly;
    }

    /** complete class symbol c by loading the unit
     */
    public String doComplete(Symbol clasz) throws IOException {
        SourceFile source = global.getSourceFile(clasz);
        Unit unit = new Unit(global, source, false, mixinOnly);
        Phase phase = global.currentPhase;
        global.currentPhase = global.PHASE.PARSER.phase();
        global.PHASE.PARSER.phase().apply(new Unit[] {unit});
        global.currentPhase = global.PHASE.ANALYZER.phase();
        ((AnalyzerPhase)global.PHASE.ANALYZER.phase()).lateEnter(global, unit, clasz);
        global.currentPhase = phase;
        return "source file '" + source.getFile() + "'";
    }

}
