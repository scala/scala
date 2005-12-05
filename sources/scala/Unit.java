/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

public abstract class Unit    extends AnyVal {

    public final void    value() {}

    public Unit   (             ) {

    }

    public boolean equals(java.lang.Object other) {
        return other instanceof Unit;
    }
    public int hashCode() {
        int  bits = 4041;
        return bits;
    }
    public String toString() {
        return "()";
    }

    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $eq$eq  (java.lang.Object other) { return  equals(other); }
    /** @meta method (scala.Any)scala.Boolean; */
    public boolean $bang$eq(java.lang.Object other) { return !equals(other); }

}
