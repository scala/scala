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

    /** other type tags (used for hashcodes and Pickling)
     */
    int ERRORtpe = 20;
    int NOtpe = 21;
    int THIStpe = 22;
    int NAMEDtpe = 23;
    int SINGLEtpe = 24;
    int COMPOUNDtpe = 25;
    int METHODtpe = 26;
    int POLYtpe = 27;
    int CONSTRUCTORtpe = 28;
    int COVARtpe = 29;
    int OVERLOADEDtpe = 30;
    int UNBOXEDtpe = 31;
    int UNBOXEDARRAYtpe = 32;

    int firstTypeTag = ERRORtpe;
    int lastTypeTag = UNBOXEDARRAYtpe;
}
