/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** The empty list.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 15/07/2003
 */
case object Nil extends List[All] {
    def isEmpty = true;
    def head: All = error("head of empty list");
    def tail: List[All] = error("tail of empty list");
}
