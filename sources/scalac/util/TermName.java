/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.util;

import java.util.HashMap;

/** Instances of this class represent term names. */
public final class TermName extends Name {

    //########################################################################
    // Private Variables

    /** Hashtable from string representation to term names. */
    private static HashMap/*<String,TermName>*/ strings = new HashMap();

    /** Hashtable from ASCII representation to term names. */
    private static TermName[] asciis = new TermName[0x00008000];

    //########################################################################
    // Private Fields

    /** The string representation */
    private final String string;

    /** The ASCII representation (null if not yet computed) */
    private byte[] ascii;

    /** The next name stored in the same bucket of the ASCII table */
    private TermName next;

    //########################################################################
    // Private Constructors

    /** Initializes this instance. */
    private TermName(String string) {
        super(2 * strings.size(), null);
        this.string = string;
        strings.put(string, this);
    }

    //########################################################################
    // Public Factories

    /** Returns the term name with given ASCII representation. */
    public static TermName fromAscii(byte[] bytes, int start, int count) {
        int index = hashValue(bytes, start, count) & (asciis.length - 1);
        for (TermName name = asciis[index]; name != null; name = name.next)
            if (name.equals(bytes, start, count)) return name;
        TermName name = fromString(
            SourceRepresentation.ascii2string(bytes, start, count));
        byte[] ascii = new byte[count];
        for (int i = 0; i < ascii.length; i++) ascii[i] = bytes[start + i];
        name.setAscii(ascii, index);
        return name;
    }

    /** Returns the term name with given string representation. */
    public static TermName fromString(String string) {
        Object value = strings.get(string);
        return value != null ? (TermName)value : new TermName(string);
    }

    //########################################################################
    // Public Methods

    /** Returns the string representation of this name. */
    public String toString() {
        return string;
    }

    /**
     * Returns the ASCII representation of this name. The returned
     * array is not a copy. Therefore, it is forbidden to modify it.
     */
    public byte[] toAsciiUnsafe() {
        if (ascii == null) {
            byte[] ascii = SourceRepresentation.string2ascii(string);
            int index = hashValue(ascii, 0, ascii.length) & (asciis.length-1);
            setAscii(ascii, index);
        }
        return ascii;
    }

    //########################################################################
    // Private Methods & Functions

    /** Sets the ASCII representation to given one. */
    private void setAscii(byte[] ascii, int index) {
        assert this.ascii == null: this;
        this.ascii = ascii;
        this.next = asciis[index];
        asciis[index] = this;
    }

    /** Is this name's ASCII representations equal to given one? */
    private boolean equals(byte[] bytes, int start, int count) {
        if (ascii.length != count) return false;
        for (int i = 0; i < count; i++)
            if (ascii[i] != bytes[start + i]) return false;
        return true;
    }

    /** Returns the hash code of the ASCII representation. */
    private static int hashValue(byte[] bytes, int start, int count) {
        if (count <= 0) return 0;
        return count * (41 * 41 * 41)
            + bytes[start] * (41 * 41)
            + bytes[start + count - 1] * 41
            + bytes[start + (count >> 1)];
    }

    //########################################################################
}
