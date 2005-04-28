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
        if (! (o == null || o instanceof scala.Char))
            throw new ClassCastException(); // TODO error message
        return o;
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
