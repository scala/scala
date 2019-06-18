/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.concurrent

/** A `SyncChannel` allows one to exchange data synchronously between
 *  a reader and a writer thread. The writer thread is blocked until the
 *  data to be written has been read by a corresponding reader thread.
 */
@deprecated("Use `java.util.concurrent.Exchanger` instead.", since = "2.13.0")
class SyncChannel[A] {

  private[this] var pendingWrites = List[(A, SyncVar[Boolean])]()
  private[this] var pendingReads  = List[SyncVar[A]]()

  def write(data: A): Unit = {
    // create write request
    val writeReq = new SyncVar[Boolean]

    this.synchronized {
      // check whether there is a reader waiting
      if (pendingReads.nonEmpty) {
        val readReq  = pendingReads.head
        pendingReads = pendingReads.tail

        // let reader continue
        readReq put data

        // resolve write request
        writeReq put true
      }
      else {
        // enqueue write request
        pendingWrites = pendingWrites ::: List((data, writeReq))
      }
    }

    writeReq.get
  }

  def read: A = {
    // create read request
    val readReq = new SyncVar[A]

    this.synchronized {
      // check whether there is a writer waiting
      if (pendingWrites.nonEmpty) {
        // read data
        val (data, writeReq) = pendingWrites.head
        pendingWrites = pendingWrites.tail

        // let writer continue
        writeReq.put(true)

        // resolve read request
        readReq.put (data)
      }
      else {
        // enqueue read request
        pendingReads = pendingReads ::: List(readReq)
      }
    }

    readReq.get
  }
}
