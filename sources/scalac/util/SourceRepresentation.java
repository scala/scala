/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id
\*                                                                      */

package scalac.util;

import java.io.File;

public final class SourceRepresentation {

    public static int digit2int(byte ch, int base) {
        if ('0' <= ch && ch <= '9' && ch < '0' + base)
            return ch - '0';
        else if ('A' <= ch && ch < 'A' + base - 10)
            return ch - 'A' + 10;
        else if ('a' <= ch && ch < 'a' + base - 10)
            return ch - 'a' + 10;
        else
            return -1;
    }

    public static byte int2digit(int x) {
        if (x <= 9)
            return (byte)(x + '0');
        else
            return (byte)(x - 10 + 'A');
    }

/* the next 4 functions convert between three fundamental name
 * representations:
 *  - string   each character 16 bit,
 *  - source   characters outside 0..127 are represented by
 *              unicode escapes, \ u X X X X
 *  - ascii   characters outside 0..127 are represented by two or three
 *              byte sequences with high bit set (as in class file format).
 */

/** convert source bytes in source[offset..offset+len-1] to ascii.
 */
    public static int source2ascii(byte source[], int offset, int len, byte ascii[]) {
        int j = 0;
        int i = 0;
        while (i < len) {
            if (source[offset + i] == '\\' && i + 1 < len) {
                i++;
                switch (source[offset + i]) {
                case 'n':
                    ascii[j++] = (byte)'\n'; i++; continue;
                case 't':
                    ascii[j++] = (byte)'\t'; i++; continue;
                case 'b':
                    ascii[j++] = (byte)'\b'; i++; continue;
                case 'r':
                    ascii[j++] = (byte)'\r'; i++; continue;
                case 'f':
                    ascii[j++] = (byte)'\f'; i++; continue;
                case 'u':
                    if (i + 4 < len) {
                        int code = 0;
                        int k = 1;
                        int d = 0;
                        while (k <= 4 && d >= 0) {
                            d = digit2int(source[offset + i + k], 16);
                            code = code * 16 + d;
                            k++;
                        }
                        if (d >= 0) {
                            if (code <= 0x7F)
                                ascii[j++] = (byte)code;
                            else
                            if (code <= 0x3FF) {
                                ascii[j++] = (byte)(0xC0 | (code >> 6));
                                ascii[j++] = (byte)(0x80 | (code & 0x3F));
                            } else {
                                ascii[j++] = (byte)(0xE0 | (code >> 12));
                                ascii[j++] = (byte)(0x80 |
                                                    ((code >> 6) & 0x3F));
                                ascii[j++] = (byte)(0x80 | (code & 0x3F));
                            }
                            i = i + 5;
                            continue;
                        }
                    }
                }
            }
            byte b = source[offset + i++];
            if (b >= 0)
                ascii[j++] = b;
            else {
                ascii[j++] = (byte)(0xC0 | ((b >> 6) & 0x3));
                ascii[j++] = (byte)(0x80 | (b & 0x3F));
            }
        }
        return j;
    }

/** convert ascii bytes in ascii[offset..offset+len-1] to a string
 */
    public static String ascii2string(byte ascii[], int offset, int len) {
        char    cs[] = new char[len];
        int     i = offset;
        int     j = 0;
        len += offset;
        while (i < len) {
            int b = ascii[i++] & 0xFF;
            if (b >= 0xE0) {
                b = ((b & 0x0F) << 12) | (ascii[i++] & 0x3F) << 6;
                b = b | (ascii[i++] & 0x3F);
            }
            else
            if (b >= 0xC0)
                b = ((b & 0x1F) << 6) | (ascii[i++] & 0x3F);
            cs[j++] = (char)b;
        }
        return new String(cs, 0, j);
    }

/** convert string to array of source bytes
 */
    public static byte[] string2source(String s) {
        byte[]  source = new byte[s.length() * 6];
        int     j = 0;
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
            switch (ch) {
            case '\n':
                source[j++] = (byte)'\\';
                source[j++] = (byte)'n';
                break;
            case '\t':
                source[j++] = (byte)'\\';
                source[j++] = (byte)'t';
                break;
            case '\b':
                source[j++] = (byte)'\\';
                source[j++] = (byte)'b';
                break;
            case '\r':
                source[j++] = (byte)'\\';
                source[j++] = (byte)'r';
                break;
            case '\f':
                source[j++] = (byte)'\\';
                source[j++] = (byte)'f';
                break;
            case '\"':
                source[j++] = (byte)'\\';
                source[j++] = (byte)'\"';
                break;
            case '\'':
                source[j++] = (byte)'\\';
                source[j++] = (byte)'\'';
                break;
            case '\\':
                source[j++] = (byte)'\\';
                source[j++] = (byte)'\\';
                break;
            default:
                if ((' ' <= ch) && (ch <= 127))
                    source[j++] = (byte)ch;
                else {
                    source[j++] = (byte)'\\';
                    source[j++] = (byte)'u';
                    source[j++] = int2digit((ch >> 12) & 0xF);
                    source[j++] = int2digit((ch >> 8) & 0xF);
                    source[j++] = int2digit((ch >> 4) & 0xF);
                    source[j++] = int2digit(ch & 0xF);
                }
            }
        }
        byte[] res = new byte[j];
        System.arraycopy(source, 0, res, 0, j);
        return res;
    }

/** convert string to array of ascii bytes
 */
    public static byte[] string2ascii(String s) {
        byte[] source = string2source(s);
        byte[] ascii = new byte[source.length * 2];
        int alen = source2ascii(source, 0, source.length, ascii);
        byte[] res = new byte[alen];
        System.arraycopy(ascii, 0, res, 0, alen);
        return res;
    }

/** escape all characters outside 32..127 in string s
 */
    public static String escape(String s) {
        try {
            return new String(string2source(s), "8859_1");
        } catch (java.io.UnsupportedEncodingException e) {
            throw new InternalError(e.getMessage());
        }
    }

/** escape character c, if outside 32..127.
 */
    public static String escape(char c) {
        char[] s = {c};
        return escape(new String(s));
    }

    /** return external representation of file name s,
     *  converting '.' to File.separatorChar
     */
    public static String externalizeFileName(Name n) {
        if ((n == null) || (n.length() == 0))
            return ".";
        byte[] ascii = n.toAscii();
        String s = ascii2string(ascii, 0, ascii.length);
        return s.replace('.', File.separatorChar);
    }
}
