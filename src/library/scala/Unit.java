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

    // prevent inheriting from the class
    private Unit() {}

    /** @meta method (scala.Any)scala.Boolean; */
    abstract public boolean $eq$eq  (java.lang.Object other);
    /** @meta method (scala.Any)scala.Boolean; */
    abstract public boolean $bang$eq(java.lang.Object other);

}
