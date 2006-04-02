/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;


public abstract class Unit extends AnyVal {

    //public final void value() {}

    private Unit() {}

    public boolean equals(java.lang.Object other) { return super.equals(other); }
    public int hashCode() { return super.hashCode(); }
    public String toString() { return super.toString();}

    /** @meta method (scala.Any)scala.Boolean; */
    abstract public boolean $eq$eq  (java.lang.Object other);
    /** @meta method (scala.Any)scala.Boolean; */
    abstract public boolean $bang$eq(java.lang.Object other);

}
