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

public class ClassType extends Type {
    public final Class clazz;
    public final boolean isTrivial;

    public ClassType(Class clazz, boolean isTrivial) {
        this.clazz = clazz;
        this.isTrivial = isTrivial;
    }

    public Array newArray(int size) {
        // TODO is that correct if we have type arguments?
        Object[] array =
            (Object[])java.lang.reflect.Array.newInstance(clazz, size);
        return RunTime.box_oarray(array);
    }

    public Object defaultValue() {
        return null;
    }

    public boolean isInstance(Object o) {
        return clazz.isInstance(o);
    }

    public boolean isSubType(Type that) {
        return (that == Type.Any)
            || (that instanceof ClassType
                && isSubClassType((ClassType)that))
            || (that instanceof CompoundType
                && isSubCompoundType((CompoundType)that));
    }

    protected boolean isSubClassType(ClassType that) {
        return that.clazz.isAssignableFrom(this.clazz);
    }

    protected final boolean isSubCompoundType(CompoundType that) {
        // TODO? check refinement
        for (int i = 0; i < that.components.length; ++i) {
            if (!isSubType(that.components[i]))
                return false;
        }

        return true;
    }

    public boolean isSameType(Type that) {
        return (that instanceof ClassType)
            && (((ClassType)that).clazz == this.clazz);
    }

    public int hashCode() {
        return clazz.hashCode();
    }

    public String toString() {
        return clazz.getName();
    }
}
