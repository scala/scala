/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

/** @meta class [?T] extends scala.AnyRef with java.lang.Cloneable with java.io.Serializable; */
public abstract class Array implements Cloneable, java.io.Serializable {

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

}
