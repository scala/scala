/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable


/** `History[A, B]` objects may subscribe to events of
 *  type `A` published by an object of type `B`.
 *  The history subscriber object records all published events
 *  up to maximum number of `maxHistory` events.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 *  @since   1
 *
 *  @tparam Evt   Type of events.
 *  @tparam Pub   Type of publishers.
 */
@SerialVersionUID(5219213543849892588L)
class History[Evt, Pub]
extends AbstractIterable[(Pub, Evt)]
   with Subscriber[Evt, Pub]
   with Iterable[(Pub, Evt)]
   with Serializable
{
  protected val log: Queue[(Pub, Evt)] = new Queue
  val maxHistory: Int = 1000

  /** Notifies this listener with an event by enqueuing it in the log.
   *
   *  @param pub   the publisher.
   *  @param event the event.
   */
  def notify(pub: Pub, event: Evt) {
    if (log.length >= maxHistory)
      log.dequeue()

    log.enqueue((pub, event))
  }

  override def size: Int = log.length
  def iterator: Iterator[(Pub, Evt)] = log.iterator
  def events: Iterator[Evt] = log.iterator map (_._2)

  def clear() { log.clear() }

  /** Checks if two history objects are structurally identical.
   *
   *  @return true, iff both history objects contain the same sequence of elements.
   */
  override def equals(obj: Any): Boolean = obj match {
    case that: History[_, _] => this.log equals that.log
    case _                   => false
  }
  override def hashCode = log.hashCode()
}
