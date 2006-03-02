/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;


/** @meta class [?T] extends java.lang.Object with java.io.Serializable;
 */
public class Ref extends java.lang.Object implements java.io.Serializable {

    /** @meta field ?T;
     */
    public java.lang.Object elem = null;

    /** @meta constr (?T);
     */
    public Ref(java.lang.Object x) {
	elem = x;
    }
}
