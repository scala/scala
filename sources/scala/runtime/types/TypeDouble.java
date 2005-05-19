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

public public class TypeDouble extends ValueType {
    private final scala.Double ZERO = RunTime.box_dvalue(0.0);
    public Object cast(Object o) {
        assert scala.runtime.types.Statistics.incTypeCast();
        if (o == null || o instanceof scala.Double)
            return o;
        else if (o instanceof scala.Float)
            return RunTime.box_dvalue(((scala.Float)o).value);
        else if (o instanceof scala.Long)
            return RunTime.box_dvalue(((scala.Long)o).value);
        else if (o instanceof scala.Int)
            return RunTime.box_dvalue(((scala.Int)o).value);
        else if (o instanceof scala.Short)
            return RunTime.box_dvalue(((scala.Short)o).value);
        else if (o instanceof scala.Char)
            return RunTime.box_dvalue(((scala.Char)o).value);
        else if (o instanceof scala.Byte)
            return RunTime.box_dvalue(((scala.Byte)o).value);
        else
            throw new ClassCastException();
    }
    public Object defaultValue() { return ZERO; }
    public boolean isSameAsJavaType(Class that) {
        return that == java.lang.Double.TYPE;
    }
    public String toString() { return "scala.Double"; }
    public int hashCode() { return 0x11111111; }

    // Make TypeDouble a serializable singleton
    public static TypeDouble INSTANCE = new TypeDouble();
    protected TypeDouble() { /* exists only to that instantiation */ }
    private Object readResolve() { return INSTANCE; }
}
