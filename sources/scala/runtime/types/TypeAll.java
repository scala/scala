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

public class TypeAll extends SpecialType {
    public boolean isInstance(Object o) {
        assert Statistics.incInstanceOf();
        return false;
    }

    public boolean isSubType(Type that) {
        return true;
    }

    public String toString() { return "scala.All"; }
    public int hashCode() { return 0xAAAAAAAA; }
};
