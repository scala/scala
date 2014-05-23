/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.concurrent

/** This class provides a simple FIFO queue of data objects,
 *  which are read by one or more reader threads.
 *
 *  @tparam A type of data exchanged
 *  @author  Martin Odersky
 *  @version 1.0, 10/03/2003
 */
class Channel[A] {
  class LinkedList[A] {
    var elem: A = _
    var next: LinkedList[A] = null
  }
  private var written = new LinkedList[A] // FIFO queue, realized through
  private var lastWritten = written       // aliasing of a linked list
  private var nreaders = 0

  /** Append a value to the FIFO queue to be read by `read`.
   *  This operation is nonblocking and can be executed by any thread.
   *
   * @param x object to enqueue to this channel
   */
  def write(x: A) = synchronized {
    lastWritten.elem = x
    lastWritten.next = new LinkedList[A]
    lastWritten = lastWritten.next
    if (nreaders > 0) notify()
  }

  /** Retrieve the next waiting object from the FIFO queue,
   *  blocking if necessary until an object is available.
   *
   * @return next object dequeued from this channel
   */
  def read: A = synchronized {
    while (written.next == null) {
      try {
        nreaders += 1
        wait()
      }
      finally nreaders -= 1
    }
    val x = written.elem
    written = written.next
    x
  }
}
