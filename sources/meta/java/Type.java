/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.java;

/** A representation for Java types. */
public class Type {

    //########################################################################
    // Public Constants

    /** The Java primitive type void */
    public static final Type VOID    = Primitive("void");

    /** The Java primitive type boolean */
    public static final Type BOOLEAN = Primitive("boolean");

    /** The Java primitive type byte */
    public static final Type BYTE    = Primitive("byte");

    /** The Java primitive type short */
    public static final Type SHORT   = Primitive("short");

    /** The Java primitive type char */
    public static final Type CHAR    = Primitive("char");

    /** The Java primitive type int */
    public static final Type INT     = Primitive("int");

    /** The Java primitive type long */
    public static final Type LONG    = Primitive("long");

    /** The Java primitive type float */
    public static final Type FLOAT   = Primitive("float");

    /** The Java primitive type double */
    public static final Type DOUBLE  = Primitive("double");

    //########################################################################
    // Public Cases

    /** A primitive type */
    public case Primitive(String name);

    /** A reference type (the owner may be null) */
    public case Reference(String owner, String name);

    /** An array type */
    public case Array(Type item);

    //########################################################################
    // Public Methods

    /** Returns the type's fully qualified name. */
    public String getFullName() {
        return getName(true);
    }

    /** Returns the type's short name. */
    public String getName() {
        return getName(false);
    }

    /** Returns the type's (possibly fully qualified) name. */
    public String getName(boolean qualified) {
        switch (this) {
        case Primitive(String name):
            return name;
        case Reference(String owner, String name):
            return qualified && owner != null ? owner + "." + name : name;
        case Array(Type item):
            return item.getName(qualified) + "[]";
        default:
            throw new Error("illegal case: " + getName(true));
        }
    }

    /** Returns the type's owner (its package or enclosing type). */
    public String getOwner() {
        switch (this) {
        case Primitive(_):
            return null;
        case Reference(String owner, _):
            return owner;
        case Array(Type item):
            return item.getOwner();
        default:
            throw new Error("illegal case: " + getName(true));
        }
    }

    /** If this is an array type, returns the type of the elements. */
    public Type getItemType() {
        switch (this) {
        case Array(Type item):
            return item;
        default:
            throw new Error("not an array type: " + getName(true));
        }
    }

    /** Returns the base type of this type. */
    public Type getBaseType() {
        return isArray() ? getItemType() : this;
    }

    /** Returns true if this is a primitive type. */
    public boolean isPrimitive() {
        switch (this) {
        case Primitive(_):
            return true;
        default:
            return false;
        }
    }

    /** Returns true if this is an array type. */
    public boolean isArray() {
        switch (this) {
        case Array(_):
            return true;
        default:
            return false;
        }
    }

    /**
     * Returns the string representation of an array instantiation
     * with the given bounds and whose elements are of this type.
     */
    public String newArray(String bounds) {
        switch (this) {
        case Array(Type item):
            return item.newArray(bounds + "[]");
        default:
            return this + bounds;
        }
    }


    /** Returns the string representation of this type. */
    public String toString() {
        return getName();
    }

    //########################################################################
}
