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

package scala.jdk.javaapi

import java.util.concurrent.CompletionStage

import scala.concurrent.impl.FutureConvertersImpl.{CF, P}
import scala.concurrent.{ExecutionContext, Future}

/** This object contains methods that convert between Scala [[Future]] and Java [[CompletionStage]].
  *
  * The explicit conversion methods defined here are intended to be used in Java code. For Scala
  * code, it is recommended to use the extension methods defined in [[scala.jdk.FutureConverters]].
  *
  * Note that the bridge is implemented at the read-only side of asynchronous handles, namely
  * [[Future]] (instead of [[scala.concurrent.Promise]]) and [[CompletionStage]] (instead of
  * [[java.util.concurrent.CompletableFuture]]). This is intentional, as the semantics of bridging
  * the write-handles would be prone to race conditions; if both ends (`CompletableFuture` and
  * `Promise`) are completed independently at the same time, they may contain different values
  * afterwards. For this reason, `toCompletableFuture` is not supported on the created
  * `CompletionStage`s.
  */
object FutureConverters {
  /** Returns a [[CompletionStage]] that will be completed with the same value or exception as the
    * given Scala [[Future]] when that completes. Since the Future is a read-only representation,
    * this CompletionStage does not support the `toCompletableFuture` method.
    *
    * The semantics of Scala Future demand that all callbacks are invoked asynchronously by default,
    * therefore the returned CompletionStage routes all calls to synchronous transformations to
    * their asynchronous counterparts, i.e., `thenRun` will internally call `thenRunAsync`.
    *
    * @param f The Scala Future which may eventually supply the completion for the returned
    *          CompletionStage
    * @return a CompletionStage that runs all callbacks asynchronously and does not support the
    *         CompletableFuture interface
    */
  def toJava[T](f: Future[T]): CompletionStage[T] = {
    f match {
      case p: P[T] => p.wrapped
      // in theory not safe (could be `class C extends Future[A] with CompletionStage[B]`):
      case c: CompletionStage[T @unchecked] => c
      case _ =>
        val cf = new CF[T](f)
        f.onComplete(cf)(ExecutionContext.parasitic)
        cf
    }
  }

  /** Returns a Scala [[Future]] that will be completed with the same value or exception as the
    * given [[CompletionStage]] when that completes. Transformations of the returned Future are
    * executed asynchronously as specified by the ExecutionContext that is given to the combinator
    * methods.
    *
    * @param cs The CompletionStage which may eventually supply the completion for the returned
    *           Scala Future
    * @return a Scala Future that represents the CompletionStage's completion
    */
  def toScala[T](cs: CompletionStage[T]): Future[T] = {
    cs match {
      case cf: CF[T] => cf.wrapped
      // in theory not safe (could be `class C extends Future[A] with CompletionStage[B]`):
      case f: Future[T @unchecked] => f
      case _ =>
        val p = new P[T](cs)
        cs whenComplete p
        p.future
    }
  }
}
