/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.typechecker;

import scalac.symtab.Symbol;
import scalac.symtab.Type;

public abstract class Infer {

    /** throw a type error if arguments not within bounds.
     */
    public abstract void checkBounds(Symbol[] tparams, Type[] targs, String prefix);

}
