/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

public final class UTF8Codec {

    public static int encode(char[] src, int from, byte[] dst, int to, int len) {
        int i = from;
        int j = to;
        int end = from + len;
        while (i < end) {
            int ch = src[i++];
            if (ch < 128)
                dst[j++] = (byte)ch;
            else if (ch <= 0x3FF) {
                dst[j++] = (byte)(0xC0 | (ch >> 6));
                dst[j++] = (byte)(0x80 | (ch & 0x3F));
            } else {
                dst[j++] = (byte)(0xE0 | (ch >> 12));
                dst[j++] = (byte)(0x80 | ((ch >> 6) & 0x3F));
                dst[j++] = (byte)(0x80 | (ch & 0x3F));
            }
        }
        return j;
    }

    public static int encode(String s, byte[] dst, int to) {
        return encode(s.toCharArray(), 0, dst, to, s.length());
    }

    public static byte[] encode(String s) {
        byte[] dst = new byte[s.length() * 3];
        int len = encode(s, dst, 0);
        byte[] result = new byte[len];
        System.arraycopy(dst, 0, result, 0, len);
        return result;
    }

    public static int decode(byte[] src, int from, char[] dst, int to, int len) {
        int i = from;
        int j = to;
        int end = from + len;
        while (i < end) {
            int b = src[i++] & 0xFF;
            if (b >= 0xE0) {
                b = ((b & 0x0F) << 12) | (src[i++] & 0x3F) << 6;
                b = b | (src[i++] & 0x3F);
            } else if (b >= 0xC0) {
                b = ((b & 0x1F) << 6) | (src[i++] & 0x3F);
            }
            dst[j++] = (char)b;
        }
	return j;
    }

    public static String decode(byte[] src, int from, int len) {
        char[] cs = new char[len];
        return String.copyValueOf(cs, 0, decode(src, 0, cs, 0, len));
    }
}
