/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.util;

/** This class implements the common part of TermName and TypeName. */
public abstract class Name {

    //########################################################################
    // Public Fields

    /** The unique identifier */
    public final int index;

    //########################################################################
    // Private Fields

    /** The related term name */
    private final TermName term;

    /** The related type name (null if not yet created) */
    private TypeName type;

    //########################################################################
    // Protected Constructors

    /** Initializes this instance. */
    protected Name(int index, TermName term) {
        this.index = index;
        this.term = term == null ? (TermName)this : term;
        this.type = term == null ? null : (TypeName)this;
    }

    //########################################################################
    // Public Factories

    /** Returns the term name with given ASCII representation. */
    public static TermName fromAscii(byte[] bytes, int start, int count) {
        return TermName.fromAscii(bytes, start, count);
    }

    /** Returns the term name with given string representation. */
    public static TermName fromString(String string) {
        return TermName.fromString(string);
    }

    //########################################################################
    // Public Methods

    /** Is this name a variable identifier? */
    public final boolean isVariable() {
        char first = charAt(0);
        return (('a' <= first && first <= 'z') || first == '_')
            && this != Names.false_
            && this != Names.true_
            && this != Names.null_;
    }

    /** Is this name a term name? */
    public final boolean isTermName() {
        return this == term;
    }

    /** Is this name a type name? */
    public final boolean isTypeName() {
        return this == type;
    }

    /** Returns the term name with the same representation. */
    public final TermName toTermName() {
        return term;
    }

    /** Returns the type name with the same representation. */
    public final TypeName toTypeName() {
        return type != null ? type : (type = new TypeName(term));
    }

    /** Returns the result of "toString().length()". */
    public final int length() {
        return toString().length();
    }

    /** Returns the result of "toString().charAt(index)". */
    public final char charAt(int index) {
        return toString().charAt(index);
    }

    /** Returns the result of "toString().indexOf(ch)". */
    public final int indexOf(char ch) {
        return toString().indexOf(ch);
    }

    /** Returns the result of "toString().indexOf(ch, start)". */
    public final int indexOf(char ch, int start) {
        return toString().indexOf(ch, start);
    }

    /** Returns the result of "toString().lastIndexOf(ch)". */
    public final int lastIndexOf(char ch) {
        return toString().lastIndexOf(ch);
    }

    /** Returns the result of "toString().lastIndexOf(ch, start)". */
    public final int lastIndexOf(char ch, int start) {
        return toString().lastIndexOf(ch, start);
    }

    /** Returns the hash code of this name. */
    public final int hashCode() {
        return index;
    }

    /** Returns the string representation of this name. */
    public String toString() {
        return term.toString();
    }

    /**
     * Returns the ASCII representation of this name. The returned
     * array is not a copy. Therefore, it is forbidden to modify it.
     */
    public byte[] toAsciiUnsafe() {
        return term.toAsciiUnsafe();
    }

    //########################################################################
}
