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
import scala.Int;

public class TypeInt extends ValueType {
    private final Int ZERO = RunTime.box_ivalue(0);
    public Array newArray(int size) {
        return RunTime.box_iarray(new int[size]);
    }
    public Object checkCastability(Object o) {
        if (! (o == null || o instanceof scala.Int))
            throw new ClassCastException(); // TODO error message
        return o;
    }
    public Object defaultValue() { return ZERO; }
    public String toString() { return "scala.Int"; }
    public int hashCode() { return 0x44444444; }
};
