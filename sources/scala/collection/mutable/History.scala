/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** <code>History[A, B]</code> objects may subscribe to events of
 *  type <code>A</code> published by an object of type <code>B</code>.
 *  The history subscriber object records all published events
 *  up to maximum number of <code>maxHistory</code> events.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
class History[A, B] with Subscriber[A, B] {

    protected val log: Queue[Pair[B, A]] = new Queue[Pair[B, A]];

    val maxHistory: Int = 32000;

    def update(pub: B, event: A): Unit = {
        if (log.length >= maxHistory) {
            val old = log.dequeue;
        }
        log.enqueue(Pair(pub, event));
    }

    def getHistory: Iterator[Pair[B, A]] = log.toList.elements;

    def historySize: Int = log.length;

    def clear: Unit = log.clear;
}
