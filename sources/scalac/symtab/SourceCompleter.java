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

    public SourceCompleter(Global global) {
        super(global);
    }

    /** complete class symbol c by loading the unit
     */
    public String doComplete(Symbol clasz) throws IOException {
        SourceFile source = global.getSourceFile(clasz);
        global.compileLate(source, false);
        return "source file '" + source + "'";
    }

}
