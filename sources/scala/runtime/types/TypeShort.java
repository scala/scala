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

public class TypeShort extends Type {
    private final scala.Short ZERO = RunTime.box_svalue((short)0);
    public Array newArray(int size) {
        return RunTime.box_sarray(new short[size]);
    }
    public Object defaultValue() { return ZERO; }
    public boolean isInstance(Object o) {
        throw new UnsupportedOperationException();
    }
    public boolean isSubType(Type that) {
        return false;           // TODO
    }
};
