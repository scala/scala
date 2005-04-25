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
import scala.runtime.RunTime;

/**
 * Abstract superclass for all "special" types, which are types
 * existing in Scala but not in Java: Any, AnyVal, All and AllRef.
 *
 * @author Michel Schinz
 * @version 1.0
 */

abstract public class SpecialType extends Type {
    public Array newArray(int size) {
        if (Type.unsafeArraysAllowed.get() == java.lang.Boolean.TRUE)
            return (Array)
                java.lang.reflect.Array.newInstance(Object.class, size);
        else
            throw new Error("cannot create arrays of special type "
                            + "(" + getClass().getName() + ")");
    }

    public Object defaultValue() {
        return null;
    }

    public boolean isSameType(Type that) {
        return this == that;
    }
}
