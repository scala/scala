/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab;


public interface Kinds {

    /** kind of non-existent symbol
     */
    int NONE = 1;

    /** definition kinds
     */
    int ALIAS = 2;
    int CLASS = 3;
    int TYPE = 4;
    int VAL = 5;
}
