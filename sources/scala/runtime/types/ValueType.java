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

/**
 * Abstract superclass for all value types.
 *
 * @author Michel Schinz
 * @version 1.0
 */

abstract public class ValueType extends Type {
    public boolean isInstance(Object o) {
        throw new UnsupportedOperationException();
    }
    public boolean isSubType(Type that) {
        return that == Type.Any
            || that == Type.AnyVal
            || that == this;
    }
    public boolean isSameAs(Type that) {
        return this == that;
    }
}
