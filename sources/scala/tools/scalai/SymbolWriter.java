/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: SymbolWriter.java,v 1.1 2002/07/12 16:59:14 paltherr Exp $
// $Id$

package scalai;

import java.io.PrintWriter;

import scalac.symtab.Symbol;

public class SymbolWriter {

    //########################################################################
    // Private Fields

    public final PrintWriter writer;

    //########################################################################
    // Public Constructors

    public SymbolWriter(PrintWriter writer) {
        this.writer = writer;
    }

    //########################################################################
    // Public Methods

    public void write1(Symbol symbol) {
        writer.println(symbol.defString());
    }

    public void write2(Symbol symbol, Object value) {
        writer.println(symbol.defString() + " = " + value);
    }

    //########################################################################
}
