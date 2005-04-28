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
import scala.Unit;

public class TypeUnit extends ValueType {
    private final Unit ZERO = RunTime.box_uvalue();
    public Object cast(Object o) {
        assert scala.runtime.types.Statistics.incTypeCast();
        if (! (o == null || o instanceof scala.Unit))
            throw new ClassCastException(); // TODO error message
        return o;
    }
    public Object defaultValue() { return ZERO; }
    public boolean isSameAsJavaType(Class that) {
        return that == java.lang.Void.TYPE;
    }
    public String toString() { return "scala.Unit"; }
    public int hashCode() { return 0x99999999; }

    // Make TypeUnit a serializable singleton
    public static TypeUnit INSTANCE = new TypeUnit();
    protected TypeUnit() { /* exists only to that instantiation */ }
    private Object readResolve() { return INSTANCE; }
}
