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

public class TypeShort extends ValueType {
    private final scala.Short ZERO = RunTime.box_svalue((short)0);
    public Object cast(Object o) {
        assert scala.runtime.types.Statistics.incTypeCast();
        if (o == null || o instanceof scala.Short)
            return o;
        else if (o instanceof scala.Double)
            return RunTime.box_svalue((short)((scala.Double)o).value);
        else if (o instanceof scala.Float)
            return RunTime.box_svalue((short)((scala.Float)o).value);
        else if (o instanceof scala.Long)
            return RunTime.box_svalue((short)((scala.Long)o).value);
        else if (o instanceof scala.Int)
            return RunTime.box_svalue((short)((scala.Int)o).value);
        else if (o instanceof scala.Char)
            return RunTime.box_svalue((short)((scala.Char)o).value);
        else if (o instanceof scala.Byte)
            return RunTime.box_svalue(((scala.Byte)o).value);
        else
            throw new ClassCastException();
    }
    public Object defaultValue() { return ZERO; }
    public boolean isSameAsJavaType(Class that) {
        return that == java.lang.Short.TYPE;
    }
    public String toString() { return "scala.Short"; }
    public int hashCode() { return 0x55555555; }

    // Make TypeShort a serializable singleton
    public static TypeShort INSTANCE = new TypeShort();
    protected TypeShort() { /* exists only to that instantiation */ }
    private Object readResolve() { return INSTANCE; }
}
