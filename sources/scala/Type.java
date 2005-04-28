/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

import scala.runtime.RunTime;
import scala.runtime.types.ScalaClassType;
import scala.runtime.types.JavaClassType;
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
import scala.runtime.types.Statistics;

import scala.runtime.FNV_Hash;
import scala.runtime.PearsonHash;

/**
 * Run-time types for Scala.
 *
 * @author Michel Schinz
 * @version 1.0
 */

abstract public class Type implements java.io.Serializable {
    public Type() {
        assert Statistics.incInstances(getClass().getName(), this);
    }

    protected static ThreadLocal unsafeArraysAllowed = new BooleanThreadLocal();

    /*
     * Allow (or not) the creation of "unsafe" arrays for the current
     * thread.
     */
    public static void allowUnsafeArrays(boolean allow) {
        unsafeArraysAllowed.set(allow
                                ? java.lang.Boolean.TRUE
                                : java.lang.Boolean.FALSE);
    }

    /** @meta method [?T](scala.Int) scala.Array[?T]; */
    abstract public Array newArray(int size);

    /** Return the default value for the type (_ in Scala) */
    abstract public Object defaultValue();

    /**
     * Return true iff the given object is an instance of a subtype of
     * this type (implement Scala's isInstanceOf operation).
     */
    abstract public boolean isInstance(Object o);

    abstract public boolean isSameType(Type that);
    abstract public boolean isSubType(Type that);

    public boolean isSameAsJavaType(Class that) {
        throw new Error("cannot compare Scala type " + this
                        + " with Java type " + that);
    }

    public boolean equals(Object that) {
        return (that instanceof Type) && this.isSameType((Type)that);
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
    public Object cast(Object o) {
        assert Statistics.incTypeCast();
        if (o == null) {
            if (this.isSubType(JavaLangObject))
                return null;
            else
                throw new ClassCastException();
        } else {
            assert Statistics.decInstanceOf();
            if (isInstance(o))
                return o;
            else
                throw new ClassCastException("\n" + ((ScalaObject)o).getScalaType()
                                             + "\n" + this.toString());
        }
    }

    // Value types
    public static final TypeDouble  Double  = TypeDouble.INSTANCE;
    public static final TypeFloat   Float   = TypeFloat.INSTANCE;
    public static final TypeLong    Long    = TypeLong.INSTANCE;
    public static final TypeInt     Int     = TypeInt.INSTANCE;
    public static final TypeShort   Short   = TypeShort.INSTANCE;
    public static final TypeChar    Char    = TypeChar.INSTANCE;
    public static final TypeByte    Byte    = TypeByte.INSTANCE;
    public static final TypeBoolean Boolean = TypeBoolean.INSTANCE;
    public static final TypeUnit    Unit    = TypeUnit.INSTANCE;

    // "Special" types
    public static final TypeAny    Any    = TypeAny.INSTANCE;
    public static final TypeAnyVal AnyVal = TypeAnyVal.INSTANCE;
    public static final TypeAllRef AllRef = TypeAllRef.INSTANCE;
    public static final TypeAll    All    = TypeAll.INSTANCE;

    private static JavaClassType JavaLangObject;

    static {
        try {
            JavaLangObject = new JavaClassType("java.lang.Object");
        } catch (ClassNotFoundException e) {
            throw new Error(e);
        }
    }

    public static boolean isSameType(Type[] these, Type[] those) {
        if (these.length != those.length)
            return false;
        for (int i = 0; i < these.length; ++i) {
            if (!these[i].isSameType(those[i]))
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

class BooleanThreadLocal extends ThreadLocal {
    protected Object initialValue() {
        return java.lang.Boolean.TRUE;
    }
}
