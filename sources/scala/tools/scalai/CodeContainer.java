/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: CodeContainer.java,v 1.1 2002/06/06 11:42:25 paltherr Exp $
// $Id$

package scalai;

import ch.epfl.lamp.util.SourceFile;

import scalac.symtab.Symbol;

public class CodeContainer {

    //########################################################################
    // Public Constructors

    public CodeContainer(
        SourceFile source, Symbol symbol, Code code, int stacksize)
    {
        this.source = source;
        this.symbol = symbol;
        this.code = code;
        this.stacksize = stacksize;
    }

    //########################################################################
    // Public Fields

    public final SourceFile source;
    public final Symbol symbol;
    public final Code code;
    public final int stacksize;

    //########################################################################
}
