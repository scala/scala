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
import scala.Byte;

public class TypeByte extends ValueType {
    private final Byte ZERO = RunTime.box_bvalue((byte)0);
    public Array newArray(int size) {
        return RunTime.box_barray(new byte[size]);
    }
    public Object cast(Object o) {
        assert scala.runtime.types.Statistics.incTypeCast();
        if (! (o == null || o instanceof scala.Byte))
            throw new ClassCastException(); // TODO error message
        return o;
    }
    public Object defaultValue() { return ZERO; }
    public String toString() { return "scala.Byte"; }
    public int hashCode() { return 0x77777777; }
};
