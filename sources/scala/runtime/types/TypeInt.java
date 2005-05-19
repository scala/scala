/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
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
    public Object cast(Object o) {
        assert scala.runtime.types.Statistics.incTypeCast();
        if (o == null || o instanceof scala.Int)
            return o;
        else if (o instanceof scala.Double)
            return RunTime.box_ivalue((int)((scala.Double)o).value);
        else if (o instanceof scala.Float)
            return RunTime.box_ivalue((int)((scala.Float)o).value);
        else if (o instanceof scala.Long)
            return RunTime.box_ivalue((int)((scala.Long)o).value);
        else if (o instanceof scala.Short)
            return RunTime.box_ivalue(((scala.Short)o).value);
        else if (o instanceof scala.Char)
            return RunTime.box_ivalue(((scala.Char)o).value);
        else if (o instanceof scala.Byte)
            return RunTime.box_ivalue(((scala.Byte)o).value);
        else
            throw new ClassCastException();
    }
    public Object defaultValue() { return ZERO; }
    public boolean isSameAsJavaType(Class that) {
        return that == java.lang.Integer.TYPE;
    }
    public String toString() { return "scala.Int"; }
    public int hashCode() { return 0x44444444; }

    // Make TypeInt a serializable singleton
    public static TypeInt INSTANCE = new TypeInt();
    protected TypeInt() { /* exists only to that instantiation */ }
    private Object readResolve() { return INSTANCE; }
}
