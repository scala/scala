/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

import scala.runtime.types.ScalaClassType;

/** @meta class [?T] extends scala.AnyRef with scala.Function1[scala.Int, ?T] with scala.ScalaObject with java.lang.Cloneable with java.io.Serializable; */
public abstract class Array
    implements Function1, ScalaObject, Cloneable, java.io.Serializable
{

    /** @meta constr (scala.Int); */
    public Array() {}

    /** @meta method []scala.Array[?T]; */
    public abstract Object value();

    /** @meta method []scala.Int; */
    public abstract int length();
    /** @meta method (scala.Int)?T; */
    public abstract Object apply(int i);
    /** @meta method (scala.Int,?T)scala.Unit; */
    public abstract void update(int i, Object x);

    /** @meta method (scala.Function1[?T,scala.Unit])scala.Unit; */
    public final void foreach(Function1 f) {
        for (int i = 0, l = length(); i < l; i++) f.apply(apply(i));
    }

    /** @meta method (scala.Function1[?T,scala.Boolean])scala.Boolean; */
    public final boolean forall(Function1 f) {
        for (int i = 0, l = length(); i < l; i++)
            if (!((scala.Boolean)f.apply(apply(i))).value) return false;
        return true;
    }

    /** @meta method (scala.Function1[?T,scala.Boolean])scala.Boolean; */
    public final boolean exists(Function1 f) {
        for (int i = 0, l = length(); i < l; i++)
            if (((scala.Boolean)f.apply(apply(i))).value) return true;
        return false;
    }

    public final boolean isDefinedAt(int i) {
        return (0 <= i) && (i < length());
    }

    public final Object apply(Object i) {
        return apply(((Int)i).value);
    }

    public ScalaClassType getType() {
        // TODO
        throw new Error("Scala arrays are not polymorphic yet...");
    }

    public int $tag() {
        return 0;
    }

}
