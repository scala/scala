/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

import scala.runtime.RunTime;
import scala.runtime.types.ScalaClassType;
import scala.runtime.types.TypeBoolean;
import scala.runtime.types.TypeByte;
import scala.runtime.types.TypeChar;
import scala.runtime.types.TypeDouble;
import scala.runtime.types.TypeFloat;
import scala.runtime.types.TypeInt;
import scala.runtime.types.TypeLong;
import scala.runtime.types.TypeShort;
import scala.runtime.types.TypeUnit;
import scala.runtime.types.TypeAll;
import scala.runtime.types.TypeAllRef;
import scala.runtime.types.TypeAny;
import scala.runtime.types.TypeAnyVal;

import scala.runtime.FNV_Hash;
import scala.runtime.PearsonHash;

/**
 * Run-time types for Scala.
 *
 * @author Michel Schinz
 * @version 1.0
 */

abstract public class Type {
    public static final Type[] EMPTY_ARRAY = new Type[]{};

    /** @meta method [?T](scala.Int) scala.Array[?T]; */
    abstract public Array newArray(int size);

    /** Return the default value for the type (_ in Scala) */
    abstract public Object defaultValue();

    /**
     * Return true iff the given object is an instance of a subtype of
     * this type (implement Scala's isInstanceOf operation).
     */
    abstract public boolean isInstance(Object o);

    abstract public boolean isSameAs(Type that);
    abstract public boolean isSubType(Type that);

    public boolean equals(Object that) {
        return (that instanceof Type) && this.isSameAs((Type)that);
    }

    public int hashCode() {
        throw new Error("missing hashCode implementation in class "
                        + this.getClass());
    }

    /**
     * Check that the given object can be cast to this type, and throw
     * an exception if this is not possible (implement Scala's
     * asInstanceOf operation).
     */
    public Object checkCastability(Object o) {
        if (! (o == null || isInstance(o)))
            throw new ClassCastException("\n" + ((ScalaObject)o).getType() + "\n" + this.toString());
        return o;
    }

    // Value types
    public static final TypeDouble  Double  = new TypeDouble();
    public static final TypeFloat   Float   = new TypeFloat();
    public static final TypeLong    Long    = new TypeLong();
    public static final TypeInt     Int     = new TypeInt();
    public static final TypeShort   Short   = new TypeShort();
    public static final TypeChar    Char    = new TypeChar();
    public static final TypeByte    Byte    = new TypeByte();
    public static final TypeBoolean Boolean = new TypeBoolean();
    public static final TypeUnit    Unit    = new TypeUnit();

    // "Special" types
    public static final TypeAny    Any    = new TypeAny();
    public static final TypeAnyVal AnyVal = new TypeAnyVal();
    public static final TypeAllRef AllRef = new TypeAllRef();
    public static final TypeAll    All    = new TypeAll();

    public static boolean isSameAs(Type[] these, Type[] those) {
        if (these.length != those.length)
            return false;
        for (int i = 0; i < these.length; ++i) {
            if (!these[i].isSameAs(those[i]))
                return false;
        }
        return true;
    }

    public static int hashCode(Type[] types) {
        final int len = types.length;

        int h = FNV_Hash.INIT;
        for (int i = 0; i < len; ++i)
            h = FNV_Hash.hashStep(h, PearsonHash.hash8(types[i].hashCode()));

        return h;
    }
}
