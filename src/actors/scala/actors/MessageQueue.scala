/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
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
class MessageQueueElement {
  var msg: Any = _
  var session: OutputChannel[Any] = null
  var next: MessageQueueElement = null
}

/**
 * The class <code>MessageQueue</code> provides an efficient
 * implementation of a message queue specialized for this actor
 * library. Classes in this package are supposed to be the only
 * clients of this class.
 *
 * @version 0.9.8
 * @author Philipp Haller
 */
@serializable
class MessageQueue {
  var first: MessageQueueElement = null
  // last == null iff list empty
  var last: MessageQueueElement = null

  def append(msg: Any, session: OutputChannel[Any]) = {
    if (null eq last) { // list empty
      val el = new MessageQueueElement
      el.msg = msg
      el.session = session
      first = el
      last = el
    }
    else {
      val el = new MessageQueueElement
      el.msg = msg
      el.session = session
      last.next = el
      last = el
    }
  }

  def extractFirst(p: Any => Boolean): MessageQueueElement = {
    if (null eq last) null
    else {
      // test first element
      if (p(first.msg)) {
        val tmp = first
        // remove first element
        first = first.next

        // might have to update last
        if (tmp eq last) {
          last = null
        }

        tmp
      }
      else {
        var curr = first
        var prev = curr
        while(curr.next != null) {
          prev = curr
          curr = curr.next
          if (p(curr.msg)) {
            // remove curr
            prev.next = curr.next

            // might have to update last
            if (curr eq last) {
              last = prev
            }

            return curr
          }
        }
        null
      }
    }
  }
}
