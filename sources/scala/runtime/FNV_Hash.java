/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime;

/**
 * Provide methods to compute the various kinds of Fowler / Noll / Vo
 * (FNV) hash.
 *
 * @author Michel Schinz
 */

public class FNV_Hash {
    public static final int INIT = -2128831035;

    public static int hashStep8(int current, int newInt8) {
        return (current * 16777619) ^ newInt8;
    }

    public static int hashStep32(int current, int newInt32) {
        final int v1 = hashStep8(current, newInt32 >> 24);
        final int v2 = hashStep8(v1, (newInt32 >> 16) & 0xFF);
        final int v3 = hashStep8(v2, (newInt32 >> 8) & 0xFF);
        return hashStep8(v3, newInt32 & 0xFF);
    }

    public static int hash32(byte[] bytes) {
        final int len = bytes.length;

        int h = INIT;
        for (int i = 0; i < len; ++i)
            h = hashStep8(h, bytes[i]);

        return h;
    }

    public static int hash32(String str) {
        try {
            return hash32(str.getBytes("UTF-8"));
        } catch (java.io.UnsupportedEncodingException e) {
            throw new Error(e);
        }
    }
}
