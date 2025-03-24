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

import java.util.concurrent.TimeUnit

/** A class to provide safe concurrent access to a mutable cell.
 *  All methods are synchronized.
 *
 *  @tparam A type of the contained value
 */
@deprecated("Use `java.util.concurrent.LinkedBlockingQueue with capacity 1` instead.", since = "2.13.0")
class SyncVar[A] {
  private[this] var isDefined: Boolean = false
  private[this] var value: A = _

  /**
   * Wait for this SyncVar to become defined and then get
   * the stored value without modifying it.
   *
   * @return value that is held in this container
   */
  def get: A = synchronized {
    while (!isDefined) wait()
    value
  }

  /** Waits `timeout` millis. If `timeout <= 0` just returns 0.
    * It never returns negative results.
    */
  private def waitMeasuringElapsed(timeout: Long): Long = if (timeout <= 0) 0 else {
    val start = System.nanoTime()
    wait(timeout)
    val elapsed = System.nanoTime() - start
    // nanoTime should be monotonic, but it's not possible to rely on that.
    // See https://bugs.java.com/view_bug.do?bug_id=6458294
    if (elapsed < 0) 0 else TimeUnit.NANOSECONDS.toMillis(elapsed)
  }

  /** Wait at least `timeout` milliseconds (possibly more) for this `SyncVar`
   *  to become defined and then get its value.
   *
   *  @param timeout     time in milliseconds to wait
   *  @return            `None` if variable is undefined after `timeout`, `Some(value)` otherwise
   */
  def get(timeout: Long): Option[A] = synchronized {
    /* Defending against the system clock going backward
     * by counting time elapsed directly.  Loop required
     * to deal with spurious wakeups.
     */
    var rest = timeout
    while (!isDefined && rest > 0) {
      val elapsed = waitMeasuringElapsed(rest)
      rest -= elapsed
    }
    if (isDefined) Some(value) else None
  }

  /**
   * Wait for this SyncVar to become defined and then get
   * the stored value, unsetting it as a side effect.
   *
   * @return value that was held in this container
   */
  def take(): A = synchronized {
    try get
    finally unsetVal()
  }

  /** Wait at least `timeout` milliseconds (possibly more) for this `SyncVar`
   *  to become defined and then get the stored value, unsetting it
   *  as a side effect.
   *
   *  @param timeout     the amount of milliseconds to wait
   *  @return            the value or a throws an exception if the timeout occurs
   *  @throws NoSuchElementException on timeout
   */
  def take(timeout: Long): A = synchronized {
    try get(timeout).get
    finally unsetVal()
  }

  /** Place a value in the SyncVar. If the SyncVar already has a stored value,
   * wait until another thread takes it. */
  def put(x: A): Unit = synchronized {
    while (isDefined) wait()
    setVal(x)
  }

  /** Check whether a value is stored in the synchronized variable. */
  def isSet: Boolean = synchronized {
    isDefined
  }

  // `setVal` exists so as to retroactively deprecate `set` without
  // deprecation warnings where we use `set` internally. The
  // implementation of `set` was moved to `setVal` to achieve this
  private def setVal(x: A): Unit = synchronized {
    isDefined = true
    value = x
    notifyAll()
  }

  // `unsetVal` exists so as to retroactively deprecate `unset` without
  // deprecation warnings where we use `unset` internally. The
  // implementation of `unset` was moved to `unsetVal` to achieve this
  private def unsetVal(): Unit = synchronized {
    isDefined = false
    value = null.asInstanceOf[A]
    notifyAll()
  }
}
