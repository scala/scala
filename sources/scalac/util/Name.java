/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.util;

import java.util.HashMap;

public final class Name {

/** address in the name memory
 */
    public final int index;

    private final String string;

/** hashtable for finding term names quickly
 */
    private static HashMap/*<String,TermName>*/ terms = new HashMap();

    private final Name term;
    private Name type;

/** Constructor
 */
    private Name(String string, Name dual) {
        this.string = string;
        this.index = dual != null ? dual.index + 1 : 2 * terms.size();
        this.term = dual != null ? dual : this;
        this.type = dual != null ? this : null;
        if (dual == null) terms.put(string, this);
    }

/** create a term name from the bytes in cs[offset..offset+len-1].
 *  assume that bytes are in ascii format.
 */
    public static Name fromAscii(byte cs[], int offset, int len) {
        return fromString(SourceRepresentation.ascii2string(cs, offset, len));
    }

/** create a name from the characters in string s
 */
    public static Name fromString(String s) {
        Object value = terms.get(s);
        if (value != null) return (Name)value;
        return new Name(s, null);
    }

/** return the string representation of this name
 */
    public String toString() {
        return string;
    }

/** is this name a term name?
 */
    public boolean isTermName() {
        return this == term;
    }

/** is this name a type name?
 */
    public boolean isTypeName() {
        return this == type;
    }

/** create a term name corresponding to this name
 */
    public Name toTermName() {
        return term;
    }

/** create a type name corresponding to this name
 */
    public Name toTypeName() {
        return type != null ? type : (type = new Name(string, this));
    }

/** return the string hash value of this name
 */
    public int hashCode() {
        return index;
    }

/** returns the length of this name
 */
    public int length() {
        return string.length();
    }

/** returns i'th char of this name
 */
    public char charAt(int i) {
        return string.charAt(i);
    }

/** returns first occurrence of char c in this name, -1 if not found
 */
    public int indexOf(char c) {
        return indexOf(c, 0);
    }

/** returns first occurrence of char c in this name from `start', -1 if not found
 */
    public int indexOf(char c, int start) {
        return string.indexOf(c, start);
    }

/** returns last occurrence of char c in this name, -1 if not found.
 */
    public int lastIndexOf(char c) {
        return string.lastIndexOf(c);
    }

/** is this name a variable identifier?
 */
    public boolean isVariable() {
        char first = string.charAt(0);
        return ((first >= 'a' && first <= 'z') || first == '_') &&
            this != Names.null_ &&
            this != Names.true_ &&
            this != Names.false_;
    }

}
