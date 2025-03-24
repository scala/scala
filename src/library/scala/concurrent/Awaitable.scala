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



import scala.concurrent.duration.Duration



/**
 * An object that may eventually be completed with a result value of type `T` which may be
 * awaited using blocking methods.
 *
 * The [[Await]] object provides methods that allow accessing the result of an `Awaitable`
 * by blocking the current thread until the `Awaitable` has been completed or a timeout has
 * occurred.
 */
trait Awaitable[+T] {

  /**
   * Await the "completed" state of this `Awaitable`.
   *
   * '''''This method should not be called directly; use [[Await.ready]] instead.'''''
   *
   * @param  atMost
   *         maximum wait time, which may be negative (no waiting is done),
   *         [[scala.concurrent.duration.Duration.Inf Duration.Inf]] for unbounded waiting, or a finite positive
   *         duration
   * @return this `Awaitable`
   * @throws InterruptedException     if the current thread is interrupted while waiting
   * @throws TimeoutException         if after waiting for the specified time this `Awaitable` is still not ready
   * @throws IllegalArgumentException if `atMost` is [[scala.concurrent.duration.Duration.Undefined Duration.Undefined]]
   */
  @throws(classOf[TimeoutException])
  @throws(classOf[InterruptedException])
  def ready(atMost: Duration)(implicit permit: CanAwait): this.type

  /**
   * Await and return the result (of type `T`) of this `Awaitable`.
   *
   * '''''This method should not be called directly; use [[Await.result]] instead.'''''
   *
   * @param  atMost
   *         maximum wait time, which may be negative (no waiting is done),
   *         [[scala.concurrent.duration.Duration.Inf Duration.Inf]] for unbounded waiting, or a finite positive
   *         duration
   * @return the result value if the `Awaitable` is completed within the specific maximum wait time
   * @throws InterruptedException     if the current thread is interrupted while waiting
   * @throws TimeoutException         if after waiting for the specified time this `Awaitable` is still not ready
   * @throws IllegalArgumentException if `atMost` is [[scala.concurrent.duration.Duration.Undefined Duration.Undefined]]
   */
  @throws(classOf[TimeoutException])
  @throws(classOf[InterruptedException])
  def result(atMost: Duration)(implicit permit: CanAwait): T
}



