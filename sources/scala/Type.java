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

    abstract public boolean isSubType(Type that);

    /**
     * Check that the given object can be cast to this type, and throw
     * an exception if this is not possible (implement Scala's
     * asInstanceOf operation).
     */
    public Object checkCastability(Object o) {
        if (! (o == null || isInstance(o)))
            throw new ClassCastException(this.toString());
        return o;
    }

    // Basic types
    public static final TypeDouble  Double  = new TypeDouble();
    public static final TypeFloat   Float   = new TypeFloat();
    public static final TypeLong    Long    = new TypeLong();
    public static final TypeInt     Int     = new TypeInt();
    public static final TypeShort   Short   = new TypeShort();
    public static final TypeChar    Char    = new TypeChar();
    public static final TypeByte    Byte    = new TypeByte();
    public static final TypeBoolean Boolean = new TypeBoolean();
}
