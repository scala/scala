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

package scala.tools.testkit.async

// The async phase expects the state machine class to structurally conform to this interface.
trait AsyncStateMachine[F, R] {
  /** Assign `i` to the state variable */
  protected def state_=(i: Int): Unit
  /** Retrieve the current value of the state variable */
  protected def state: Int
  /** Complete the state machine with the given failure. */
  protected def completeFailure(t: Throwable): Unit
  /** Complete the state machine with the given value. */
  protected def completeSuccess(value: AnyRef): Unit
  /** Register the state machine as a completion callback of the given future. */
  protected def onComplete(f: F): Unit
  /** Extract the result of the given future if it is complete, or `null` if it is incomplete. */
  protected def getCompleted(f: F): R
  /**
   * Extract the success value of the given future. If the state machine detects a failure it may
   * complete the async block and return `this` as a sentinel value to indicate that the caller
   * (the state machine dispatch loop) should immediately exit.
   */
  protected def tryGet(tr: R): AnyRef
}
