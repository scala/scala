/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.util;

public abstract class ArrayApply {

    //########################################################################
    // Arrays interface - Object

    public static String toString(Object[] src) {
        return append(new StringBuffer(), src).toString();
    }

    public static String toString(Object[] src, String infix) {
        return append(new StringBuffer(), src, infix).toString();
    }

    public static String toString(Object[] src, String prefix, String infix,
        String suffix)
    {
        return append(new StringBuffer(), src, prefix,infix,suffix).toString();
    }

    //########################################################################
    // Arrays interface - StringBuffer

    public static StringBuffer append(StringBuffer buffer, Object[] src) {
        return append(buffer, src, "[", ",", "]");
    }

    public static StringBuffer append(StringBuffer buffer, Object[] src,
        String infix)
    {
        return append(buffer, src, "", infix, "");
    }

    public static StringBuffer append(StringBuffer buffer, Object[] src,
        String prefix, String infix, String suffix)
    {
        buffer.append(prefix);
        for (int i = 0; i < src.length; i++) {
            if (i > 0) buffer.append(infix);
            buffer.append(src[i]);
        }
        buffer.append(suffix);
        return buffer;
    }
}
