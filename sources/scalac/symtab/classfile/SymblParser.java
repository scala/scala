/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import java.io.IOException;

import scala.tools.util.AbstractFile;

import scalac.Global;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolLoader;

/** This class implements a SymbolLoader that reads a symbol file. */
public class SymblParser extends SymbolLoader {

    //########################################################################
    // Private Fields

    /** The symbol file to read */
    private final AbstractFile file;

    //########################################################################
    // Public Constructors

    /** Initializes this instance with the specified symbol file. */
    public SymblParser(Global global, AbstractFile file) {
	super(global);
        this.file = file;
    }

    //########################################################################
    // Protected Methods

    /** Completes the specified symbol by reading the symbol file. */
    public String doComplete(Symbol clasz) throws IOException {
        UnPickle.parse(global, file, clasz);
        return "symbol file '" + file + "'";
    }

    //########################################################################
}
