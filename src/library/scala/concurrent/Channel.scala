/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent

/** This class ...
 *
 *  @author  Martin Odersky
 *  @version 1.0, 10/03/2003
 */
class Channel[a] {
  class LinkedList[a] {
    var elem: a = _
    var next: LinkedList[a] = null
  }
  private var written = new LinkedList[a] // FIFO buffer, realized through
  private var lastWritten = written       // aliasing of a linked list
  private var nreaders = 0

  /**
   *  @param x ...
   */
  def write(x: a) = synchronized {
    lastWritten.elem = x
    lastWritten.next = new LinkedList[a]
    lastWritten = lastWritten.next
    if (nreaders > 0) notify()
  }

  def read: a = synchronized {
    while (null == written.next) {
      nreaders = nreaders + 1; wait(); nreaders = nreaders - 1
    }
    val x = written.elem
    written = written.next
    x
  }
}
