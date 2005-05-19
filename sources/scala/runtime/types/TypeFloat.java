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

public class TypeFloat extends ValueType {
    private final scala.Float ZERO = RunTime.box_fvalue(0.0f);
    public Object cast(Object o) {
        assert scala.runtime.types.Statistics.incTypeCast();
        if (o == null || o instanceof scala.Float)
            return o;
        else if (o instanceof scala.Double)
            return RunTime.box_fvalue((float)((scala.Double)o).value);
        else if (o instanceof scala.Long)
            return RunTime.box_fvalue(((scala.Long)o).value);
        else if (o instanceof scala.Int)
            return RunTime.box_fvalue(((scala.Int)o).value);
        else if (o instanceof scala.Short)
            return RunTime.box_fvalue(((scala.Short)o).value);
        else if (o instanceof scala.Char)
            return RunTime.box_fvalue(((scala.Char)o).value);
        else if (o instanceof scala.Byte)
            return RunTime.box_fvalue(((scala.Byte)o).value);
        else
            throw new ClassCastException();
    }
    public Object defaultValue() { return ZERO; }
    public boolean isSameAsJavaType(Class that) {
        return that == java.lang.Float.TYPE;
    }
    public String toString() { return "scala.Float"; }
    public int hashCode() { return 0x22222222; }

    // Make TypeFloat a serializable singleton
    public static TypeFloat INSTANCE = new TypeFloat();
    protected TypeFloat() { /* exists only to that instantiation */ }
    private Object readResolve() { return INSTANCE; }
}
