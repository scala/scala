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
import scala.Char;

public class TypeChar extends ValueType {
    private final Char ZERO = RunTime.box_cvalue((char)0);
    public Array newArray(int size) {
        return RunTime.box_carray(new char[size]);
    }
    public Object checkCastability(Object o) {
        assert scala.runtime.types.Statistics.incTypeCast();
        if (! (o == null || o instanceof scala.Char))
            throw new ClassCastException(); // TODO error message
        return o;
    }
    public Object defaultValue() { return ZERO; }
    public String toString() { return "scala.Char"; }
    public int hashCode() { return 0x66666666; }
};
