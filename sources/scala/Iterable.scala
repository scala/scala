/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;


/**
 * Collection classes supporting this trait provide a method
 * <code>elements</code> which returns an iterator over all the
 * elements contained in the collection.
 *
 * @author  Matthias Zenger
 * @version 1.0, 16/07/2003
 */
trait Iterable[+A] {

    /**
     * Creates a new iterator over all elements contained in this
     * object.
     *
     * @return the new iterator
     */
    def elements: Iterator[A];

}
