/* TODO: Reintegrate
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** <code>History[A, B]</code> objects may subscribe to events of
 *  type <code>A</code> published by an object of type <code>B</code>.
 *  The history subscriber object records all published events
 *  up to maximum number of <code>maxHistory</code> events.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
@serializable
class History[A, B] extends AnyRef with Subscriber[A, B] with Collection[(B, A)] {

  protected val log: Queue[(B, A)] = new Queue[(B, A)]

  val maxHistory: Int = 1000

  /**
   *  @param pub   ...
   *  @param event ...
   */
  def notify(pub: B, event: A): Unit = {
    if (log.length >= maxHistory) {
      val old = log.dequeue;
    }
    log.enqueue((pub, event))
  }

  def elements: Iterator[(B, A)] = log.elements

  def events: Iterator[A] = log.elements.map { case (_, e) => e }

  override def size: Int = log.length

  def clear(): Unit = log.clear
}
*/
