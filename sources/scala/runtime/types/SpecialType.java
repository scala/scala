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
        return RunTime.box_oarray(new Object[size]);
    }

    public Object defaultValue() {
        return null;
    }

    public boolean isSameType(Type that) {
        return this == that;
    }
}
