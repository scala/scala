/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** <tt>Subscriber[-A, -B]</tt> objects may subscribe to events of
 *  type <tt>A</tt> published by an object of type <tt>B</tt>.
 */
class RevertableHistory[A <: Undo, B] extends History[A, B] with Undo {

    def undo: Unit = {
    	val old = log.toList.reverse;
    	clear;
    	old.foreach {
    		case Pair(sub, event) => event.undo;
    	}
    }
}
