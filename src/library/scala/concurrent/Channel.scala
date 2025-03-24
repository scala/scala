/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.concurrent

/** This class provides a simple FIFO queue of data objects,
 *  which are read by one or more reader threads.
 *
 *  @tparam A type of data exchanged
 */
@deprecated("Use `java.util.concurrent.LinkedTransferQueue` instead.", since = "2.13.0")
class Channel[A] {
  private class LinkedList {
    var elem: A = _
    var next: LinkedList = _
  }
  private[this] var written = new LinkedList    // FIFO queue, realized through
  private[this] var lastWritten = written       // aliasing of a linked list
  private[this] var nreaders = 0

  /** Append a value to the FIFO queue to be read by `read`.
   *  This operation is nonblocking and can be executed by any thread.
   *
   * @param x object to enqueue to this channel
   */
  def write(x: A): Unit = synchronized {
    lastWritten.elem = x
    lastWritten.next = new LinkedList
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
