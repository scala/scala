/*
 * Hex.java
 *
 * Created 04.07.2003.
 *
 * eaio: UUID - an implementation of the UUID specification Copyright (c) 2003-2009 Johann Burkard (jb@eaio.com)
 * http://eaio.com.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
package com.eaio.util.lang;

import java.io.IOException;

/**
 * Number-to-hexadecimal and hexadecimal-to-number conversions.
 *
 * @see <a href="http://johannburkard.de/software/uuid/">UUID</a>
 * @author <a href="mailto:jb@eaio.com">Johann Burkard</a>
 * @version $Id: Hex.java 1888 2009-03-15 12:43:24Z johann $
 */
public final class Hex {

    /**
     * No instances needed.
     */
    private Hex() {
        super();
    }

    private static final char[] DIGITS = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e',
            'f' };

    /**
     * Turns a <code>short</code> into hex octets.
     *
     * @param a the {@link Appendable}, may not be <code>null</code>
     * @param in the integer
     * @return {@link Appendable}
     */
    public static Appendable append(Appendable a, short in) {
        return append(a, (long) in, 4);
    }

    /**
     * Turns a <code>short</code> into hex octets.
     *
     * @param a the {@link Appendable}, may not be <code>null</code>
     * @param in the integer
     * @param length the number of octets to produce
     * @return {@link Appendable}
     */
    public static Appendable append(Appendable a, short in, int length) {
        return append(a, (long) in, length);
    }

    /**
     * Turns an <code>int</code> into hex octets.
     *
     * @param a the {@link Appendable}, may not be <code>null</code>
     * @param in the integer
     * @return {@link Appendable}
     */
    public static Appendable append(Appendable a, int in) {
        return append(a, (long) in, 8);
    }

    /**
     * Turns an <code>int</code> into hex octets.
     *
     * @param a the {@link Appendable}, may not be <code>null</code>
     * @param in the integer
     * @param length the number of octets to produce
     * @return {@link Appendable}
     */
    public static Appendable append(Appendable a, int in, int length) {
        return append(a, (long) in, length);
    }

    /**
     * Turns a <code>long</code> into hex octets.
     *
     * @param a the {@link Appendable}, may not be <code>null</code>
     * @param in the long
     * @return {@link Appendable}
     */
    public static Appendable append(Appendable a, long in) {
        return append(a, in, 16);
    }

    /**
     * Turns a <code>long</code> into hex octets.
     *
     * @param a the {@link Appendable}, may not be <code>null</code>
     * @param in the long
     * @param length the number of octets to produce
     * @return {@link Appendable}
     */
    public static Appendable append(Appendable a, long in, int length) {
        try {
            int lim = (length << 2) - 4;
            while (lim >= 0) {
                a.append(DIGITS[(byte) (in >> lim) & 0x0f]);
                lim -= 4;
            }
        }
        catch (IOException ex) {
            // Bla
        }
        return a;
    }

    /**
     * Turns a <code>byte</code> array into hex octets.
     *
     * @param a the {@link Appendable}, may not be <code>null</code>
     * @param bytes the <code>byte</code> array
     * @return {@link Appendable}
     */
    public static Appendable append(Appendable a, byte[] bytes) {
        try {
            for (byte b : bytes) {
                a.append(DIGITS[(byte) ((b & 0xF0) >> 4)]);
                a.append(DIGITS[(byte) (b & 0x0F)]);
            }
        }
        catch (IOException ex) {
            // Bla
        }
        return a;
    }

    /**
     * Parses a <code>long</code> from a hex encoded number. This method will skip all characters that are not 0-9,
     * A-F and a-f.
     * <p>
     * Returns 0 if the {@link CharSequence} does not contain any interesting characters.
     *
     * @param s the {@link CharSequence} to extract a <code>long</code> from, may not be <code>null</code>
     * @return a <code>long</code>
     * @throws NullPointerException if the {@link CharSequence} is <code>null</code>
     */
    public static long parseLong(CharSequence s) {
        long out = 0;
        byte shifts = 0;
        char c;
        for (int i = 0; i < s.length() && shifts < 16; i++) {
            c = s.charAt(i);
            if ((c > 47) && (c < 58)) {
                ++shifts;
                out <<= 4;
                out |= c - 48;
            }
            else if ((c > 64) && (c < 71)) {
                ++shifts;
                out <<= 4;
                out |= c - 55;
            }
            else if ((c > 96) && (c < 103)) {
                ++shifts;
                out <<= 4;
                out |= c - 87;
            }
        }
        return out;
    }

    /**
     * Parses a <code>short</code> from a hex encoded number. This method will skip all characters that are not 0-9,
     * A-F and a-f.
     * <p>
     * Returns 0 if the {@link CharSequence} does not contain any interesting characters.
     *
     * @param s the {@link CharSequence} to extract a <code>short</code> from, may not be <code>null</code>
     * @return a <code>short</code>
     * @throws NullPointerException if the {@link CharSequence} is <code>null</code>
     */
    public static short parseShort(String s) {
        short out = 0;
        byte shifts = 0;
        char c;
        for (int i = 0; i < s.length() && shifts < 4; i++) {
            c = s.charAt(i);
            if ((c > 47) && (c < 58)) {
                ++shifts;
                out <<= 4;
                out |= c - 48;
            }
            else if ((c > 64) && (c < 71)) {
                ++shifts;
                out <<= 4;
                out |= c - 55;
            }
            else if ((c > 96) && (c < 103)) {
                ++shifts;
                out <<= 4;
                out |= c - 87;
            }
        }
        return out;
    }

}
