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

public class TypeLong extends ValueType {
    private final scala.Long ZERO = RunTime.box_lvalue(0l);
    public Object cast(Object o) {
        assert scala.runtime.types.Statistics.incTypeCast();
        if (o == null || o instanceof scala.Long)
            return o;
        else if (o instanceof scala.Double)
            return RunTime.box_lvalue((long)((scala.Double)o).value);
        else if (o instanceof scala.Float)
            return RunTime.box_lvalue((long)((scala.Float)o).value);
        else if (o instanceof scala.Int)
            return RunTime.box_lvalue(((scala.Int)o).value);
        else if (o instanceof scala.Short)
            return RunTime.box_lvalue(((scala.Short)o).value);
        else if (o instanceof scala.Char)
            return RunTime.box_lvalue(((scala.Char)o).value);
        else if (o instanceof scala.Byte)
            return RunTime.box_lvalue(((scala.Byte)o).value);
        else
            throw new ClassCastException();
    }
    public Object defaultValue() { return ZERO; }
    public boolean isSameAsJavaType(Class that) {
        return that == java.lang.Long.TYPE;
    }
    public String toString() { return "scala.Long"; }
    public int hashCode() { return 0x33333333; }

    // Make TypeLong a serializable singleton
    public static TypeLong INSTANCE = new TypeLong();
    protected TypeLong() { /* exists only to that instantiation */ }
    private Object readResolve() { return INSTANCE; }
}
