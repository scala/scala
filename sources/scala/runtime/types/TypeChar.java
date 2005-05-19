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
import scala.Char;

public class TypeChar extends ValueType {
    private final Char ZERO = RunTime.box_cvalue((char)0);
    public Object cast(Object o) {
        assert scala.runtime.types.Statistics.incTypeCast();
        if (o == null || o instanceof scala.Char)
            return o;
        else if (o instanceof scala.Double)
            return RunTime.box_cvalue((char)((scala.Double)o).value);
        else if (o instanceof scala.Float)
            return RunTime.box_cvalue((char)((scala.Float)o).value);
        else if (o instanceof scala.Long)
            return RunTime.box_cvalue((char)((scala.Long)o).value);
        else if (o instanceof scala.Int)
            return RunTime.box_cvalue((char)((scala.Int)o).value);
        else if (o instanceof scala.Short)
            return RunTime.box_cvalue((char)((scala.Short)o).value);
        else if (o instanceof scala.Byte)
            return RunTime.box_cvalue((char)((scala.Byte)o).value);
        else
            throw new ClassCastException();
    }
    public Object defaultValue() { return ZERO; }
    public boolean isSameAsJavaType(Class that) {
        return that == char.class;
    }
    public String toString() { return "scala.Char"; }
    public int hashCode() { return 0x66666666; }

    // Make TypeChar a serializable singleton
    public static TypeChar INSTANCE = new TypeChar();
    protected TypeChar() { /* exists only to that instantiation */ }
    private Object readResolve() { return INSTANCE; }
}
