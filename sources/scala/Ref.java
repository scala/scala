/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $OldId: Ref.java,v 1.2 2002/03/12 13:16:04 zenger Exp $
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
