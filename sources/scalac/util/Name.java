/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.util;

public final class Name {

/** address in the name memory
 */
    public int index;

/** length of the name
 */
    private int len;

/** next name in the same hash bucket
 */
    private Name next;

    private final static int HASH_SIZE = 0x8000;
    private final static int HASH_MASK = 0x7FFF;
    private final static int NAME_SIZE = 0x20000;

/** memory to store all names sequentially
 */
    public static byte[] names = new byte[NAME_SIZE];
    private static int nc = 0;

/** hashtable for finding term names quickly
 */
    private static Name[] termHashtable = new Name[HASH_SIZE];

/** hashtable for finding type names quickly
 */
    private static Name[] typeHashtable = new Name[HASH_SIZE];

/** hashtable for finding constructor names quickly
 */
    private static Name[] constrHashtable = new Name[HASH_SIZE];

/** Constructor
 */
    Name(int index, int len, Name[] table, int hash) {
	this.index = index;
	this.len = len;
	this.next = table[hash];
	table[hash] = this;
    }

/** the hashcode of a name
 */
    private static int hashValue(byte cs[], int offset, int len) {
        if (len > 0)
            return
                len * (41 * 41 * 41) +
                cs[offset] * (41 * 41) +
                cs[offset + len - 1] * 41 +
                cs[offset + (len >> 1)];
        else
            return 0;
    }

/** is (the ascii representation of) name equal to
 *  cs[offset..offset+len-1]?
 */
    private static boolean equals(int index, byte cs[], int offset, int len) {
        int i = 0;
        while ((i < len) && (names[index + i] == cs[offset + i]))
            i++;
        return i == len;
    }

/** the empty name
 */
    public static final Name EMPTY = fromString("");

/** create a term name from the bytes in cs[offset..offset+len-1].
 *  assume that bytes are in ascii format.
 */
    public static Name fromAscii(byte cs[], int offset, int len) {
        int     h = hashValue(cs, offset, len) & HASH_MASK;
        Name    n = termHashtable[h];
        while ((n != null) && (n.len != len || !equals(n.index, cs, offset, len)))
            n = n.next;
        if (n == null) {
            n = new Name(nc, len, termHashtable, h);
            for (int i = 0; i < len; i++)
            {
                if (nc == names.length)
                {
                    byte[] newnames = new byte[names.length * 2];
                    System.arraycopy(names, 0, newnames, 0, names.length);
                    names = newnames;
                }
                names[nc++] = cs[offset + i];
            }
            if (len == 0)
                nc++;
        }
        return n;
    }

/** create a name from the bytes in cs[offset..offset+len-1];
 *  assume that characters are in source format
 */
    public static Name fromSource(byte cs[], int offset, int len) {
        byte[]  ascii = new byte[len * 2];
        int alen = SourceRepresentation.source2ascii(cs, offset, len, ascii);
        return fromAscii(ascii, 0, alen);
    }

/** create a name from the characters in string s
 */
    public static Name fromString(String s) {
        byte[] source = SourceRepresentation.string2source(s);
        return fromSource(source, 0, source.length);
    }

/** copy bytes of this name to buffer cs, starting at offset
 */
    public void copyAscii(byte cs[], int offset) {
        System.arraycopy(names, index, cs, offset, len);
    }

/** return the ascii representation of this name
 */
    public byte[] toAscii() {
        byte[]  ascii = new byte[len];
        System.arraycopy(names, index, ascii, 0, len);
        return ascii;
    }

/** return the source representation of this name
 */
    public byte[] toSource() {
        return SourceRepresentation.string2source(toString());
    }

/** return the string representation of this name
 */
    public String toString() {
	return SourceRepresentation.ascii2string(names, index, len);
    }

/** is this name a term name?
 */
    public boolean isTermName() {
	int  h = hashValue(names, index, len) & HASH_MASK;
	Name n = termHashtable[h];
        while (n != null && n != this)
	    n = n.next;
	return n == this;
    }

/** is this name a type name?
 */
    public boolean isTypeName() {
	int  h = hashValue(names, index, len) & HASH_MASK;
	Name n = typeHashtable[h];
        while (n != null && n != this)
	    n = n.next;
	return n == this;
    }

/** is this name a type name?
 */
    public boolean isConstrName() {
	int  h = hashValue(names, index, len) & HASH_MASK;
	Name n = constrHashtable[h];
        while (n != null && n != this)
	    n = n.next;
	return n == this;
    }

/** create a term name corresponding to this name
 */
    public Name toTermName() {
	int  h = hashValue(names, index, len) & HASH_MASK;
        Name n = termHashtable[h];
        while (n != null && n.index != index)
            n = n.next;
	if (n == null) {
	    n = new Name(index, len, termHashtable, h);
	}
	return n;
    }

/** create a type name corresponding to this name
 */
    public Name toTypeName() {
	int  h = hashValue(names, index, len) & HASH_MASK;
        Name n = typeHashtable[h];
        while (n != null && n.index != index)
            n = n.next;
	if (n == null) {
	    n = new Name(index, len, typeHashtable, h);
	}
	return n;
    }

/** create a constructor name corresponding to this name
 */
    public Name toConstrName() {
	int  h = hashValue(names, index, len) & HASH_MASK;
        Name n = constrHashtable[h];
        while (n != null && n.index != index)
            n = n.next;
	if (n == null) {
	    n = new Name(index, len, constrHashtable, h);
	}
	return n;
    }

/** return the string hash value of this name
 */
    public int hashCode() {
        return index;
    }

/** returns the length of this name
 */
    public int length() {
        return len;
    }

/** returns i'th byte of this name
 */
    public byte sub(int i) {
        return names[index + i];
    }

/** returns first occurrence of byte b in this name, len if not found
 */
    public int pos(byte b) {
	return pos(b, 0);
    }

/** returns first occurrence of byte b in this name from `start', len if not found
 */
    public int pos(byte b, int start) {
        int i = start;
        while (i < len && names[index + i] != b)
            i++;
        return i;
    }

/** returns last occurrence of byte b in this name, -1 if not found.
 */
    public int lastPos(byte b) {
        int i = len - 1;
        while (i >= 0 && names[index + i] != b)
            i--;
        return i;
    }

/** does this name start with prefix?
 */
    public boolean startsWith(Name prefix) {
        int i = 0;
        while ((i < prefix.len) &&
                (i < len) &&
                (names[index + i] == names[prefix.index + i]))
            i++;
        return i == prefix.len;
    }

/** does this name end with suffix?
 */
    public boolean endsWith(Name suffix) {
        int i = len - 1;
        int j = suffix.len - 1;
        while ((j >= 0) && (i >= 0) &&
                (names[index + i] == names[suffix.index + j])) {
                i--;
                j--;
        }
        return j < 0;
    }

/** returns the subName starting at position start, excluding position end
 */
    public Name subName(int start, int end) {
        if (end < start)
            end = start;
        byte[]  ascii = new byte[end - start];
        System.arraycopy(names, index + start, ascii, 0, end - start);
        return fromAscii(ascii, 0, ascii.length);
    }

/** replace all `from' characters with `to'
 */
    public Name replace(byte from, byte to) {
        byte[] ascii = new byte[len];
        copyAscii(ascii, 0);
        for (int i = 0; i < len; i++)
            if (ascii[i] == from) ascii[i] = to;
        return fromAscii(ascii, 0, len);
    }

/** returns the concatenation of this name and n
 */
    public Name append(Name n) {
        byte[] ascii = new byte[len + n.len];
        copyAscii(ascii, 0);
        n.copyAscii(ascii, len);
        return fromAscii(ascii, 0, ascii.length);
    }

/** returns the concatenation of all names in ns
 */
    public static Name concat(Name ns[]) {
        int len = 0;
        for (int i = 0; i < ns.length; i++)
            len = len + ns[i].len;
        byte[] ascii = new byte[len];
        len = 0;
        for (int i = 0; i < ns.length; i++) {
            ns[i].copyAscii(ascii, len);
            len = len + ns[i].len;
        }
        return fromAscii(ascii, 0, len);
    }

/** is this name an operator?
 */
    public boolean isOperator() {
        return !(((names[index] >= 'a') && (names[index] <= 'z')) ||
                 ((names[index] >= 'A') && (names[index] <= 'Z')) ||
                 (names[index] == '$') ||
                 (names[index] == '_'));
    }

/** is this name a variable identifier?
 */
    public boolean isVariable() {
        return ((names[index] >= 'a' && names[index] <= 'z') ||
		names[index] == '_') &&
	    this != Names.null_ &&
	    this != Names.true_ &&
	    this != Names.false_;
    }

    public static final Name ERROR = Name.fromString("<error>");

/** precedence of this name
 */
    public int precedence() {
        if (this == ERROR)
            return -1;
        byte ch = names[index];
        if (((ch >= 'A') && (ch <= 'Z')) ||
            ((ch >= 'a') && (ch <= 'z')))
            return 1;
        switch (ch) {
            case '|':
                return 2;
            case '^':
                return 3;
            case '&':
                return 4;
            case '<':
            case '>':
                return 5;
            case '=':
            case '!':
                return 6;
            case ':':
                return 7;
            case '+':
            case '-':
                return 8;
            case '*':
            case '/':
            case '%':
                return 9;
            default:
                return 10;
        }
    }

    public static final int TOP_PRECEDENCE = 11;

/** is this operator left associative
 */
    public boolean isLeftAssoc() {
        return names[index + len -1] != ':';
    }
}
