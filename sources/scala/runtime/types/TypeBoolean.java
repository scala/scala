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
import scala.Boolean;

public class TypeBoolean extends ValueType {
    private final Boolean ZERO = RunTime.box_zvalue(false);
    public Array newArray(int size) {
        return RunTime.box_zarray(new boolean[size]);
    }
    public Object checkCastability(Object o) {
        if (! (o == null || o instanceof scala.Boolean))
            throw new ClassCastException(); // TODO error message
        return o;
    }
    public Object defaultValue() { return ZERO; }
    public String toString() { return "scala.Boolean"; }
    public int hashCode() { return 0x88888888; }
};
