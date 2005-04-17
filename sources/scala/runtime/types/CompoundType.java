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
    public final boolean emptyRefinement;

    public CompoundType(ClassType[] components, boolean emptyRefinement) {
        this.components = components;
        this.emptyRefinement = emptyRefinement;
    }

    public Array newArray(int size) {
        throw new Error("cannot create arrays of compound types");
    }

    public Object defaultValue() {
        return null;
    }

    public boolean isInstance(Object o) {
        assert Statistics.incInstanceOf();
        for (int i = 0; i < components.length; ++i) {
            if (!components[i].isInstance(o))
                return false;
        }

        return true;
    }

    public boolean isSubType(Type that) {
        if (!emptyRefinement)
            throw new Error("cannot compute isSubType (non-empty refinement)");

        if (that instanceof CompoundType) {
            CompoundType thatCT = (CompoundType)that;
            ClassType[] thatComponents = thatCT.components;
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

    public boolean isSameType(Type that) {
        if (!emptyRefinement)
            throw new Error("cannot compute isSameType (non-empty refinement)");

        if (that instanceof CompoundType) {
            CompoundType thatCT = (CompoundType)that;

            if (components.length != thatCT.components.length)
                return false;

            for (int i = 0; i < components.length; ++i) {
                if (!components[i].isSameType(thatCT.components[i]))
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
