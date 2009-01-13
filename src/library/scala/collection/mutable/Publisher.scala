/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** <code>Publisher[A,This]</code> objects publish events of type <code>A</code>
 *  to all registered subscribers. When subscribing, a subscriber may specify
 *  a filter which can be used to constrain the number of events sent to the
 *  subscriber. Subscribers may suspend their subscription, or reactivate a
 *  suspended subscription. Class <code>Publisher</code> is typically used
 *  as a mixin. The type variable <code>This</code> models self types.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait Publisher[A, This <: Publisher[A, This]] { self: This =>
  private val filters = new HashMap[Subscriber[A, This],
                                    scala.collection.mutable.Set[A => Boolean]]
                            with MultiMap[Subscriber[A, This], A => Boolean]
  private val suspended = new HashSet[Subscriber[A, This]]

  def subscribe(sub: Subscriber[A, This]): Unit =
    subscribe(sub, event => true)

  def subscribe(sub: Subscriber[A, This], filter: A => Boolean): Unit =
    filters.add(sub, filter)

  def suspendSubscription(sub: Subscriber[A, This]): Unit = suspended += sub

  def activateSubscription(sub: Subscriber[A, This]): Unit = suspended -= sub

  def removeSubscription(sub: Subscriber[A, This]): Unit = filters -= sub

  def removeSubscriptions() { filters.clear }

  protected def publish(event: A): Unit =
    filters.keys.foreach(sub =>
      if (filters.entryExists(sub, p => p(event))) sub.notify(this, event))
}
