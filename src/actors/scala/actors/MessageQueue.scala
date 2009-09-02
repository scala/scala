/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/**
 * This class is used by our efficient message queue
 * implementation.
 *
 * @version 0.9.9
 * @author Philipp Haller
 */
@serializable
class MessageQueueElement(var msg: Any, var session: OutputChannel[Any], var next: MessageQueueElement) {
  def this() = this(null, null, null)
  def this(msg: Any, session: OutputChannel[Any]) = this(msg, session, null)
}

object MessageQueue {
  // for tracing purposes
  private var queueNumberAssigner = 0
  private def getQueueNumber = synchronized {
    queueNumberAssigner += 1
    queueNumberAssigner
  }
}

/**
 * The class <code>MessageQueue</code> provides an efficient
 * implementation of a message queue specialized for this actor
 * library. Classes in this package are supposed to be the only
 * clients of this class.
 *
 * @version 0.9.9
 * @author Philipp Haller
 */
@serializable
class MessageQueue(label: String) {
  def this() = this("Unlabelled")
  private val queueNumber = MessageQueue.getQueueNumber
  private var trace = false // set to true to print out all appends/removes

  private var first: MessageQueueElement = null
  private var last: MessageQueueElement = null  // last eq null iff list is empty
  private var _size = 0

  def size = _size
  final def isEmpty = last eq null

  protected def changeSize(diff: Int) = {
    _size += diff
  }

  def append(msg: Any, session: OutputChannel[Any]) {
    changeSize(1) // size always increases by 1
    if (trace)
      printQueue("APPEND %s" format msg)

    val el = new MessageQueueElement(msg, session)

    if (isEmpty) first = el
    else last.next = el

    last = el
  }

  def foreach(f: (Any, OutputChannel[Any]) => Unit) {
    var curr = first
    while (curr != null) {
      f(curr.msg, curr.session)
      curr = curr.next
    }
  }

  def foldLeft[B](z: B)(f: (B, Any) => B): B = {
    var acc = z
    var curr = first
    while (curr != null) {
      acc = f(acc, curr.msg)
      curr = curr.next
    }
    acc
  }

  /** Returns the n-th msg that satisfies the predicate
   *  without removing it.
   */
  def get(n: Int)(p: Any => Boolean): Option[Any] = {
    var pos = 0

    def test(msg: Any): Boolean =
      p(msg) && (pos == n || { pos += 1; false })

    var curr = first
    while (curr != null)
      if (test(curr.msg)) {
        if (trace)
          printQueue("GET %s" format curr.msg)

        return Some(curr.msg) // early return
      }
      else curr = curr.next

    None
  }

  /** Removes the n-th msg that satisfies the predicate.
   */
  def remove(n: Int)(p: Any => Boolean): Option[(Any, OutputChannel[Any])] =
    removeInternal(n)(p) map (x => (x.msg, x.session))

  def extractFirst(p: Any => Boolean): MessageQueueElement =
    removeInternal(0)(p) orNull

  private def removeInternal(n: Int)(p: Any => Boolean): Option[MessageQueueElement] = {
    var pos = 0

    def foundMsg(x: MessageQueueElement) = {
      if (trace)
        printQueue("REMOVE %s" format x.msg)

      changeSize(-1)
      Some(x)
    }
    def test(msg: Any): Boolean =
      p(msg) && (pos == n || { pos += 1 ; false })

    if (isEmpty)    // early return
      return None

    // special handling if returning the head
    if (test(first.msg)) {
      val res = first
      first = first.next
      if (res eq last)
        last = null

      foundMsg(res)
    }
    else {
      var curr = first.next   // init to element #2
      var prev = first

      while (curr != null) {
        if (test(curr.msg)) {
          prev.next = curr.next
          if (curr eq last)
            last = prev

          return foundMsg(curr) // early return
        }
        else {
          prev = curr
          curr = curr.next
        }
      }
      // not found
      None
    }
  }

  private def printQueue(msg: String) = {
    def firstMsg = if (first eq null) "null" else first.msg
    def lastMsg = if (last eq null) "null" else last.msg

    println("[%s size=%d] [%s] first = %s, last = %s".format(this, size, msg, firstMsg, lastMsg))
  }
  override def toString() = "%s:%d".format(label, queueNumber)
}