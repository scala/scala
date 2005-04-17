/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime.types;

import scala.Type;
import scala.Array;

/**
 * Type for Java arrays of references.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JavaRefArrayType extends Type {
    public final int dimensions;
    public final Type elemType;

    public static JavaRefArrayType javaRefArrayType(Type elemType,
                                                    int dimensions) {
        if (elemType instanceof JavaRefArrayType) {
            JavaRefArrayType other = (JavaRefArrayType)elemType;
            return new JavaRefArrayType(other.elemType,
                                        dimensions + other.dimensions);
        } else
            return new JavaRefArrayType(elemType, dimensions);
    }

    private JavaRefArrayType(Type elemType, int dimensions) {
        this.elemType = elemType;
        this.dimensions = dimensions;
    }

    public Array newArray(int size) {
        throw new Error();      // TODO
    }

    public Object defaultValue() {
        return null;
    }

    public boolean isInstance(Object o) {
        assert Statistics.incInstanceOf();
        return this.isSameAsJavaType(o.getClass());
    }

    public boolean isSameType(Type that) {
        return (that instanceof JavaRefArrayType)
            && (elemType.isSameType(((JavaRefArrayType)that).elemType));
    }

    public boolean isSameAsJavaType(Class that) {
        Class thatElemType = that;
        for (int i = 0; i < dimensions && thatElemType != null; ++i)
            thatElemType = thatElemType.getComponentType();

        return (thatElemType != null)
            && (elemType.isSameAsJavaType(thatElemType));
    }

    public boolean isSubType(Type that) {
        return isSameType(that);
    }

    public String toString() {
        return elemType.toString() + "[]";
    }

    public int hashCode() {
        return elemType.hashCode() * 11;
    }
}
