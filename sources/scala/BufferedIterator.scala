/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** Buffered iterators are iterators which allow to inspect the next
 *  element without discarding it.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
trait BufferedIterator[+A] extends Iterator[A] {

    /** Checks what the next available element is.
     *
     *  @returns the current element
     */
    def head: A;
}
