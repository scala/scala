/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab;

import java.io.IOException;

import scala.tools.util.AbstractFile;

import scalac.Global;
import scalac.symtab.Symbol;

/** This class implements a SymbolLoader that reads a source file. */
public class SourceCompleter extends SymbolLoader {

    //########################################################################
    // Private Fields

    /** The source file to read */
    private final AbstractFile file;

    //########################################################################
    // Public Constructors

    /** Initializes this instance with the specified source file. */
    public SourceCompleter(Global global, AbstractFile file) {
        super(global);
        this.file = file;
    }

    //########################################################################
    // Protected Methods

    /** Completes the specified symbol by reading the source file. */
    protected String doComplete(Symbol root) throws IOException {
        global.compileLate(global.getSourceFile(file), false);
        return "source file '" + file + "'";
    }

    //########################################################################
}
