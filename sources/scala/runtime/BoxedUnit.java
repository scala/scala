/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.runtime;

public final class BoxedUnit {

    public final static BoxedUnit UNIT = new BoxedUnit();

    private BoxedUnit() { }

    public final boolean $eq$eq(java.lang.Object other) {
        return equals(other);
    }

    public final boolean $bang$eq(java.lang.Object other) {
        return !equals(other);
    }

    public boolean equals(java.lang.Object other) {
	return this == other;
    }

    public int hashCode() {
	return 0;
    }

    public String toString() {
	return "()";
    }
}
