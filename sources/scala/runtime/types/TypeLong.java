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

public class TypeLong extends ValueType {
    private final scala.Long ZERO = RunTime.box_lvalue(0l);
    public Array newArray(int size) {
        return RunTime.box_larray(new long[size]);
    }
    public Object checkCastability(Object o) {
        if (! (o == null || o instanceof scala.Long))
            throw new ClassCastException(); // TODO error message
        return o;
    }
    public Object defaultValue() { return ZERO; }
    public String toString() { return "scala.Long"; }
    public int hashCode() { return 0x33333333; }
};
