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

class SingleType extends Type {
    private final Object instance;

    public SingleType(Object instance) {
        this.instance = instance;
    }

    public Array newArray(int size) {
        throw new Error();      // TODO
    }

    public Object defaultValue() {
        return null;
    }

    public boolean isInstance(Object o) {
        throw new Error("not implemented yet"); // TODO
    }

    public boolean isSubType(Type that) {
        throw new Error("not implemented yet"); // TODO
    }

    public boolean isSameAs(Type that) {
        return (that instanceof SingleType)
            && (this.instance == ((SingleType)that).instance);
    }

    public int hashCode() {
        return instance.hashCode();
    }
}
