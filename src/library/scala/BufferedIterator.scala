/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id:BufferedIterator.scala 5359 2005-12-16 16:33:49 +0100 (Fri, 16 Dec 2005) dubochet $
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
     *  @return the current element
     */
    def head: A;
}
