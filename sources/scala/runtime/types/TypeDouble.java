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

public public class TypeDouble extends ValueType {
    private final scala.Double ZERO = RunTime.box_dvalue(0.0);
    public Array newArray(int size) {
        return RunTime.box_darray(new double[size]);
    }
    public Object checkCastability(Object o) {
        if (! (o == null || o instanceof scala.Double))
            throw new ClassCastException(); // TODO error message
        return o;
    }
    public Object defaultValue() { return ZERO; }
    public String toString() { return "scala.Double"; }
    public int hashCode() { return 0x11111111; }
};

