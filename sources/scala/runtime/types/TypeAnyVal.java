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

public class TypeAnyVal extends SpecialType {
    public boolean isInstance(Object o) {
        throw new UnsupportedOperationException();
    }

    public boolean isSubType(Type that) {
        return that == Type.Any || that == this;
    }

    public String toString() { return "scala.AnyVal"; }

    public int hashCode() { return 0xCCCCCCCC; }
};
