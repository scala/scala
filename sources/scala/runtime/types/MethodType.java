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

public class MethodType extends Type {
    public final Type[] argTypes;
    public final Type returnType;

    public MethodType(Type[] argTypes, Type returnType) {
        this.argTypes = argTypes;
        this.returnType = returnType;
    }

    public Array newArray(int size) {
        throw new Error();      // TODO provide a message (and maybe
                                // use a different exception)
    }

    public Object defaultValue() {
        throw new Error();      // TODO provide a message (and maybe
                                // use a different exception)
    }

    public boolean isInstance(Object o) {
        throw new Error();      // TODO provide a message (and maybe
                                // use a different exception)
    }

    public boolean isSubType(Type that) {
        if (that instanceof MethodType) {
            MethodType thatMT = (MethodType)that;

            if (argTypes.length != thatMT.argTypes.length)
                return false;

            for (int i = 0; i < argTypes.length; ++i) {
                if (! argTypes[i].isSameType(thatMT.argTypes[i]))
                    return false;
            }
            return returnType.isSubType(thatMT.returnType);
        } else
            return false;
    }

    public boolean isSameType(Type that) {
        if (that instanceof MethodType) {
            MethodType thatMT = (MethodType)that;

            if (argTypes.length != thatMT.argTypes.length)
                return false;

            for (int i = 0; i < argTypes.length; ++i) {
                if (! argTypes[i].isSameType(thatMT.argTypes[i]))
                    return false;
            }
            return returnType.isSameType(thatMT.returnType);
        }
        else
            return false;
    }

    public int hashCode() {
        throw new Error();      // TODO provide a message (and maybe
                                // use a different exception)
    }
}
