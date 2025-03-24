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

package scala.tools.nsc
package interactive

/** Typical interaction, given a predicate <user-input>, a function <display>,
 *  and an exception handler <handle>:
 *
 *  val TIMEOUT = 100 // (milliseconds) or something like that
 *  val r = new Response()
 *  while (!r.isComplete && !r.isCancelled) {
 *    if (<user-input>) r.cancel()
 *    else r.get(TIMEOUT) match {
 *      case Some(Left(data)) => <display>(data)
 *      case Some(Right(exc)) => <handle>(exc)
 *      case None =>
 *    }
 *  }
 */
class Response[T] {

  private var data: Option[Either[T, Throwable]] = None
  private var complete = false
  private var cancelled = false

  /** Set provisional data, more to come
   */
  def setProvisionally(x: T) = synchronized {
    data = Some(Left(x))
  }

  /** Set final data, and mark response as complete.
   */
  def set(x: T) = synchronized {
    data = Some(Left(x))
    complete = true
    notifyAll()
  }

  /** Store raised exception in data, and mark response as complete.
   */
  def raise(exc: Throwable) = synchronized {
    data = Some(Right(exc))
    complete = true
    notifyAll()
  }

  /** Get final data, wait as long as necessary.
   *  When interrupted will return with Right(InterruptedException)
   */
  def get: Either[T, Throwable] = synchronized {
    while (!complete) {
      try {
        wait()
      } catch {
        case exc: InterruptedException => {
          Thread.currentThread().interrupt()
          raise(exc)
        }
      }
    }
    data.get
  }

  /** Optionally get data within `timeout` milliseconds.
   *  When interrupted will return with Some(Right(InterruptedException))
   *  When timeout ends, will return last stored provisional result,
   *  or else None if no provisional result was stored.
   */
  def get(timeout: Long): Option[Either[T, Throwable]] = synchronized {
    val start = System.currentTimeMillis
    var current = start
    while (!complete && start + timeout > current) {
      try {
        wait(timeout - (current - start))
      } catch {
        case exc: InterruptedException => {
          Thread.currentThread().interrupt()
          raise(exc)
        }
      }
      current = System.currentTimeMillis
    }
    data
  }

  /** Final data set was stored
   */
  def isComplete = synchronized { complete }

  /** Cancel action computing this response (Only the
   *  party that calls get on a response may cancel).
   */
  def cancel() = synchronized { cancelled = true }

  /** A cancel request for this response has been issued
   */
  def isCancelled = synchronized { cancelled }

  def clear() = synchronized {
    data = None
    complete = false
    cancelled = false
  }
}
