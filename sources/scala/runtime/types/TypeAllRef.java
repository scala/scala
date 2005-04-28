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

public class TypeAllRef extends SpecialType {
    public boolean isInstance(Object o) {
        assert Statistics.incInstanceOf();
        return false;
    }

    public boolean isSubType(Type that) {
        return !that.isSubType(Type.AnyVal);
    }

    public String toString() { return "scala.AllRef"; }

    public int hashCode() { return 0xDDDDDDDD; }

    // Make TypeAllRef a serializable singleton
    public static TypeAllRef INSTANCE = new TypeAllRef();
    protected TypeAllRef() { /* exists only to that instantiation */ }
    private Object readResolve() { return INSTANCE; }
}
