/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** A non empty list characterized by a head and a tail.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 15/07/2003
 */
final case class ::[b](hd: b, tl: List[b]) extends List[b] {
    def isEmpty: boolean = false;
    def head: b = hd;
    def tail: List[b] = tl;
}
