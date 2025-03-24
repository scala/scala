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


/** A `DelayedLazyVal` is a wrapper for lengthy computations which have a
 *  valid partially computed result.
 *
 *  The first argument is a function for obtaining the result at any given
 *  point in time, and the second is the lengthy computation.  Once the
 *  computation is complete, the `apply` method will stop recalculating it
 *  and return a fixed value from that point forward.
 *
 *  @param  f      the function to obtain the current value at any point in time
 *  @param  body   the computation to run to completion in another thread
 */
@deprecated("`DelayedLazyVal` Will be removed in the future.", since = "2.13.0")
class DelayedLazyVal[T](f: () => T, body: => Unit)(implicit exec: ExecutionContext){
  @volatile private[this] var _isDone = false
  private[this] lazy val complete = f()

  /** Whether the computation is complete.
   *
   *  @return true if the computation is complete.
   */
  def isDone: Boolean = _isDone

  /** The current result of f(), or the final result if complete.
   *
   *  @return the current value
   */
  def apply(): T = if (isDone) complete else f()

  exec.execute(() => {
    body; _isDone = true
  })
}
