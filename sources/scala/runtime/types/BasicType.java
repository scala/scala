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
 * Abstract superclass for all basic types.
 *
 * @author Michel Schinz
 * @version 1.0
 */

abstract public class BasicType extends Type {
    public boolean isInstance(Object o) {
        throw new UnsupportedOperationException();
    }
    public boolean isSubType(Type that) {
        return false;           // TODO
    }
    public boolean isSameType(Type that) {
        return this == that;
    }
}
