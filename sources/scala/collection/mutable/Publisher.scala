/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** <tt>Publisher[A, This]</tt> objects publish events of type <tt>A</tt>
 *  to all registered subscribers.
 */
class Publisher[A, This <: Publisher[A, This]]: This {
    private val filters = new HashMap[Subscriber[A, This], MutableSet[A => Boolean]]
                            with MultiMap[Subscriber[A, This], A => Boolean];
    private val suspended = new HashSet[Subscriber[A, This]];

    def subscribe(sub: Subscriber[A, This]): Unit = subscribe(sub, (event => true));

    def subscribe(sub: Subscriber[A, This], filter: A => Boolean): Unit =
        filters.add(sub, filter);

    def suspendSubscription(sub: Subscriber[A, This]): Unit = suspended.add(sub);

    def activateSubscription(sub: Subscriber[A, This]): Unit = suspended.remove(sub);

    def removeSubscription(sub: Subscriber[A, This]): Unit = filters.remove(sub);

    def removeSubscriptions: Unit = filters.clear;

    protected def publish(event: A): Unit =
        filters.keys.foreach(sub =>
            if (filters.exists(sub, (p => p(event)))) sub.update(this, event));
}
