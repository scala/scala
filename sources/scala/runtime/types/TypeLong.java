/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime.types;

import scala.runtime.RunTime;
import scala.Type;
import scala.Array;

public class TypeLong extends Type {
    private final scala.Long ZERO = RunTime.box_lvalue(0l);
    public Array newArray(int size) {
        return RunTime.box_larray(new long[size]);
    }
    public Object defaultValue() { return ZERO; }
    public boolean isInstance(Object o) {
        throw new UnsupportedOperationException();
    }
    public boolean isSubType(Type that) {
        return false;           // TODO
    }
};
