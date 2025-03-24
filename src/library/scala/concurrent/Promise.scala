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

import scala.util.{ Try, Success, Failure }

/** Promise is an object which can be completed with a value or failed
 *  with an exception.
 *
 *  A promise should always eventually be completed, whether for success or failure, 
 *  in order to avoid unintended resource retention for any associated Futures' 
 *  callbacks or transformations.
 *
 *  @define promiseCompletion
 *  If the promise has already been fulfilled, failed or has timed out,
 *  calling this method will throw an IllegalStateException.
 *
 *  @define allowedThrowables
 *  If the throwable used to fail this promise is an error, a control exception
 *  or an interrupted exception, it will be wrapped as a cause within an
 *  `ExecutionException` which will fail the promise.
 *
 *  @define nonDeterministic
 *  Note: Using this method may result in non-deterministic concurrent programs.
 */
trait Promise[T] {
  /** Future containing the value of this promise.
   */
  def future: Future[T]

  /** Returns whether the promise has already been completed with
   *  a value or an exception.
   *
   *  $nonDeterministic
   *
   *  @return    `true` if the promise is already completed, `false` otherwise
   */
  def isCompleted: Boolean

  /** Completes the promise with either an exception or a value.
   *
   *  @param result     Either the value or the exception to complete the promise with.
   *
   *  $promiseCompletion
   */
  def complete(result: Try[T]): this.type =
    if (tryComplete(result)) this else throw new IllegalStateException("Promise already completed.")

  /** Tries to complete the promise with either a value or the exception.
   *
   *  $nonDeterministic
   *
   *  @return    If the promise has already been completed returns `false`, or `true` otherwise.
   */
  def tryComplete(result: Try[T]): Boolean

  /** Completes this promise with the specified future, once that future is completed.
   *
   *  @return   This promise
   */
   def completeWith(other: Future[T]): this.type = {
    if (other ne this.future) // this tryCompleteWith this doesn't make much sense
      other.onComplete(this tryComplete _)(ExecutionContext.parasitic)

    this
  }

  /** Attempts to complete this promise with the specified future, once that future is completed.
   *
   *  @return   This promise
   */
  @deprecated("Since this method is semantically equivalent to `completeWith`, use that instead.", "2.13.0")
  final def tryCompleteWith(other: Future[T]): this.type = completeWith(other)

  /** Completes the promise with a value.
   *
   *  @param value The value to complete the promise with.
   *
   *  $promiseCompletion
   */
  def success(value: T): this.type = complete(Success(value))

  /** Tries to complete the promise with a value.
   *
   *  $nonDeterministic
   *
   *  @return    If the promise has already been completed returns `false`, or `true` otherwise.
   */
  def trySuccess(value: T): Boolean = tryComplete(Success(value))

  /** Completes the promise with an exception.
   *
   *  @param cause    The throwable to complete the promise with.
   *
   *  $allowedThrowables
   *
   *  $promiseCompletion
   */
  def failure(cause: Throwable): this.type = complete(Failure(cause))

  /** Tries to complete the promise with an exception.
   *
   *  $nonDeterministic
   *
   *  @return    If the promise has already been completed returns `false`, or `true` otherwise.
   */
  def tryFailure(cause: Throwable): Boolean = tryComplete(Failure(cause))
}

object Promise {
  /** Creates a promise object which can be completed with a value.
   *
   *  @tparam T       the type of the value in the promise
   *  @return         the newly created `Promise` instance
   */
  final def apply[T](): Promise[T] = new impl.Promise.DefaultPromise[T]()

  /** Creates an already completed Promise with the specified exception.
   *
   *  @tparam T       the type of the value in the promise
   *  @return         the newly created `Promise` instance
   */
  final def failed[T](exception: Throwable): Promise[T] = fromTry(Failure(exception))

  /** Creates an already completed Promise with the specified result.
   *
   *  @tparam T       the type of the value in the promise
   *  @return         the newly created `Promise` instance
   */
  final def successful[T](result: T): Promise[T] = fromTry(Success(result))

  /** Creates an already completed Promise with the specified result or exception.
   *
   *  @tparam T       the type of the value in the promise
   *  @return         the newly created `Promise` instance
   */
  final def fromTry[T](result: Try[T]): Promise[T] = new impl.Promise.DefaultPromise[T](result)
}
