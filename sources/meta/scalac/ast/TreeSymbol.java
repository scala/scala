/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

/** This class describes the symbol stored in some tree nodes. */
public class TreeSymbol {

    //########################################################################
    // Public Cases

    /** Indicates the absence of symbol. */
    public case NoSym;

    /** Indicates the presence of a symbol. */
    public case HasSym(TreeField field, boolean isDef);

    //########################################################################
}
