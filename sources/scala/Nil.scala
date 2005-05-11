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

/** The empty list.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 15/07/2003
 */
case object Nil extends List[All] with Serializable {
    private val serialVersionUID = 0 - 8256821097970055419L;
    def isEmpty = true;
    def head: All = error("head of empty list");
    def tail: List[All] = error("tail of empty list");
}
