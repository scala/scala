/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

import java.io.Serializable;

import Predef._;

/** A non empty list characterized by a head and a tail.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 15/07/2003
 */
final case class ::[b](hd: b, tl: List[b]) extends List[b] with Serializable {
    private val serialVersionUID = 0 - 8476791151983527571L;
    def isEmpty: boolean = false;
    def head: b = hd;
    def tail: List[b] = tl;
}
