/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;


/** @meta class extends scala.AnyVal; */
public abstract class Boolean extends AnyVal {

    // prevent inheriting from the class
    private Boolean() {}

    public boolean equals(java.lang.Object other) { return super.equals(other); }
    public int hashCode() { return super.hashCode(); }
    public String toString() { return super.toString(); }

    /** @meta method (scala.Any)scala.Boolean; */
    abstract public boolean $eq$eq  (java.lang.Object other);
    /** @meta method (scala.Any)scala.Boolean; */
    abstract public boolean $bang$eq(java.lang.Object other);

    /** @meta method []scala.Boolean; */
    abstract public boolean $bang();

    abstract public boolean $eq$eq  (boolean that);
    abstract public boolean $bang$eq(boolean that);
    abstract public boolean $bar$bar(boolean that);
    abstract public boolean $amp$amp(boolean that);
    abstract public boolean $bar    (boolean that);
    abstract public boolean $amp    (boolean that);
    abstract public boolean $up     (boolean that);

}
