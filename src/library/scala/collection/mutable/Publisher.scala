/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable


/** <code>Publisher[A,This]</code> objects publish events of type <code>A</code>
 *  to all registered subscribers. When subscribing, a subscriber may specify
 *  a filter which can be used to constrain the number of events sent to the
 *  subscriber. Subscribers may suspend their subscription, or reactivate a
 *  suspended subscription. Class <code>Publisher</code> is typically used
 *  as a mixin. The type variable <code>This</code> models self types.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 *  @since   1
 */
trait Publisher[A, This <: Publisher[A, This]] {
  self: This =>

  type SubThis = Subscriber[A, This]
  type Filter = A => Boolean

  private val filters = new HashMap[SubThis, Set[Filter]] with MultiMap[SubThis, Filter]
  private val suspended = new HashSet[SubThis]

  def subscribe(sub: SubThis) { subscribe(sub, event => true) }
  def subscribe(sub: SubThis, filter: Filter) { filters(sub) += filter }
  def suspendSubscription(sub: SubThis) { suspended += sub }
  def activateSubscription(sub: SubThis) { suspended -= sub }
  def removeSubscription(sub: SubThis) { filters -= sub }
  def removeSubscriptions() { filters.clear }

  protected def publish(event: A) {
    filters.keysIterator.foreach(sub =>
      if (filters.entryExists(sub, p => p(event)))
        sub.notify(this, event)
    )
  }

  /** Checks if two publishers are structurally identical.
   *
   *  @return true, iff both publishers contain the same sequence of elements.
   */
  override def equals(obj: Any): Boolean = obj match {
    case that: Publisher[_, _] =>
      (this.filters equals that.filters) &&
      (this.suspended equals that.suspended)
    case _ =>
      false
  }

}
