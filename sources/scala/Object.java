/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $OldId: Object.java,v 1.7 2002/03/12 13:32:46 zenger Exp $
// $Id$

package scala;

/** @meta class extends scala.AnyRef;
 */
public class Object {

    /** This method is needed for optimizing pattern matching expressions
     *  which match on constructors of case classes.
     */
    public int $tag() {
    	return 0;
    }
}
