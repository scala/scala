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

public class SingleType extends Type {
    private final Object instance;

    public SingleType(Object instance) {
        this.instance = instance;
    }

    public Array newArray(int size) {
        if (Type.unsafeArraysAllowed.get() == java.lang.Boolean.TRUE)
            return (Array)
                java.lang.reflect.Array.newInstance(instance.getClass(), size);
        else
            throw new Error("cannot create array of single types");
    }

    public Object defaultValue() {
        return null;
    }

    public boolean isInstance(Object o) {
        assert Statistics.incInstanceOf();
        return o == instance;
    }

    public boolean isSubType(Type that) {
        return that.isInstance(instance);
    }

    public boolean isSameType(Type that) {
        return (that instanceof SingleType)
            && (this.instance == ((SingleType)that).instance);
    }

    public int hashCode() {
        return System.identityHashCode(instance);
    }
}
