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
import scala.ScalaObject;
import scala.runtime.RunTime;

/**
 * Run-time representation for compound types.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class CompoundType extends Type {
    public final ClassType[] components;

    public CompoundType(ClassType[] components, boolean emptyRefinement) {
        this.components = components;

        if (!emptyRefinement)
            throw new Error("attempt to build a compound type with "
                            + "non-empty refinement");
    }

    public Array newArray(int size) {
        throw new Error();      // TODO
    }

    public Object defaultValue() {
        return null;
    }

    public boolean isInstance(Object o) {
        for (int i = 0; i < components.length; ++i) {
            if (!components[i].isInstance(o))
                return false;
        }

        return true;
    }

    public boolean isSubType(Type that) {
        if (that instanceof CompoundType) {
            ClassType[] thatComponents = ((CompoundType)that).components;
            for (int i = 0; i < thatComponents.length; ++i) {
                if (!this.isSubType(thatComponents[i]))
                    return false;
            }
            return true;
        } else {
            for (int i = 0; i < components.length; ++i) {
                if (components[i].isSubType(that))
                    return true;
            }
            return false;
        }
    }

    public boolean isSameAs(Type that) {
        if (that instanceof CompoundType) {
            CompoundType thatCT = (CompoundType)that;

            if (components.length != thatCT.components.length)
                return false;

            for (int i = 0; i < components.length; ++i) {
                if (!components[i].isSameAs(thatCT.components[i]))
                    return false;
            }

            return true;
        } else
            return false;
    }

    public int hashCode() {
        return Type.hashCode(components);
    }

    public String toString() {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < components.length; ++i) {
            if (i > 0) buf.append(" with ");
            buf.append(components[i]);
        }
        return buf.toString();
    }
}
