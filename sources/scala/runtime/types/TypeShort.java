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

public class TypeShort extends ValueType {
    private final scala.Short ZERO = RunTime.box_svalue((short)0);
    public Array newArray(int size) {
        return RunTime.box_sarray(new short[size]);
    }
    public Object checkCastability(Object o) {
        assert scala.runtime.types.Statistics.incTypeCast();
        if (! (o == null || o instanceof scala.Short))
            throw new ClassCastException(); // TODO error message
        return o;
    }
    public Object defaultValue() { return ZERO; }
    public String toString() { return "scala.Short"; }
    public int hashCode() { return 0x55555555; }
};
