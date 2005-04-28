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
import scala.Boolean;

public class TypeBoolean extends ValueType {
    private final Boolean ZERO = RunTime.box_zvalue(false);
    public Object cast(Object o) {
        assert scala.runtime.types.Statistics.incTypeCast();
        if (! (o == null || o instanceof scala.Boolean))
            throw new ClassCastException(); // TODO error message
        return o;
    }
    public Object defaultValue() { return ZERO; }
    public boolean isSameAsJavaType(Class that) {
        return that == java.lang.Boolean.TYPE;
    }
    public String toString() { return "scala.Boolean"; }
    public int hashCode() { return 0x88888888; }

    // Make TypeBoolean a serializable singleton
    public static TypeBoolean INSTANCE = new TypeBoolean();
    protected TypeBoolean() { /* exists only to that instantiation */ }
    private Object readResolve() { return INSTANCE; }
}
