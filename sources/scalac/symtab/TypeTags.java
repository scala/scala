/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab;

public interface TypeTags {

    /** unboxed type tags
     */
    int BYTE = 10;
    int CHAR = 11;
    int SHORT = 12;
    int INT = 13;
    int LONG = 14;
    int FLOAT = 15;
    int DOUBLE = 16;
    int BOOLEAN = 17;
    int UNIT = 18;
    int STRING = 19;

    int FirstUnboxedTag = BYTE;
    int LastUnboxedTag = UNIT;
}
