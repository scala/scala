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
@SerialVersionUID(5219213543849892588L)
class History[A, B] extends AnyRef with Subscriber[A, B] with Iterable[(B, A)]
{
  protected val log: Queue[(B, A)] = new Queue[(B, A)]
  val maxHistory: Int = 1000

  /**
   *  @param pub   ...
   *  @param event ...
   */
  def notify(pub: B, event: A) {
    if (log.length >= maxHistory)
      log.dequeue

    log.enqueue((pub, event))
  }

  override def size: Int = log.length
  def iterator: Iterator[(B, A)] = log.iterator
  def events: Iterator[A] = log.iterator.map { case (_, e) => e }

  def clear() { log.clear }

  /** Checks if two history objects are structurally identical.
   *
   *  @return true, iff both history objects contain the same sequence of elements.
   */
  override def equals(obj: Any): Boolean = obj match {
    case that: History[_, _] => this.log equals that.log
    case _                   => false
  }
}
