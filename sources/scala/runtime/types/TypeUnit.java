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
import scala.Unit;

public class TypeUnit extends ValueType {
    private final Unit ZERO = RunTime.box_uvalue();
    public Array newArray(int size) {
        return RunTime.box_oarray(new Object[size]);
    }
    public Object checkCastability(Object o) {
        if (! (o == null || o instanceof scala.Unit))
            throw new ClassCastException(); // TODO error message
        return o;
    }
    public Object defaultValue() { return ZERO; }
    public String toString() { return "scala.Unit"; }
    public int hashCode() { return 0x99999999; }
};
