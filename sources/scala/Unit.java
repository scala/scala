/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

/**
 * Scala's Unit class.
 */

public abstract class Unit extends AnyVal {
    public void asUnit() {
    }

    public boolean equals(java.lang.Object obj) {
        return obj instanceof Unit;
    }

    public int hashCode() {
        return 4041;
    }

    public String toString() {
        return "()";
    }
}
