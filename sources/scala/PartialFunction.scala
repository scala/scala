/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2003, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;


/**
 * A partial function of type <code>PartialFunction[A, B]</code> is a
 * unary function where the domain does not include all values of type
 * <code>A</code>. The function <code>isDefinedAt</code> allows to
 * test dynamically, if a value is in the domain of the function.
 *
 * @author  Martin Odersky
 * @version 1.0, 16/07/2003
 */
trait PartialFunction[-A, +B] with Function1[A, B] {

    /**
     * Checks if a value is contained in the functions domain.
     *
     * @param  x   the value to test
     * @return true, iff <code>x</code> is in the domain of this function.
     */
    def isDefinedAt(x: A): Boolean;

}
