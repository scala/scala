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
    public final Refinement[] refinements;

    public CompoundType(ClassType[] components,
                        Refinement[] refinements) {
        this.components = components;
        this.refinements = refinements;
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

        return (refinements.length == 0)
            || (o instanceof ScalaObject
                && hasCoarserRefinements((((ScalaObject)o).getType())
                                         .refinements));
    }

    private boolean hasCoarserRefinements(Refinement[] thatRefinements) {
        return Refinement.isFiner(thatRefinements, this.refinements);
    }

    public boolean isSubType(Type that) {
        throw new Error();      // TODO
    }

    public boolean isSameAs(Type that) {
        if (that instanceof CompoundType) {
            CompoundType thatCT = (CompoundType)that;

            if ((components.length != thatCT.components.length)
                || (refinements.length != thatCT.refinements.length))
                return false;

            for (int i = 0; i < components.length; ++i) {
                if (!components[i].isSameAs(thatCT.components[i]))
                    return false;
            }

            for (int i = 0; i < refinements.length; ++i) {
                if (!refinements[i].isSameAs(thatCT.refinements[i]))
                    return false;
            }

            return true;
        } else
            return false;
    }
}
