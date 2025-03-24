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

package scala.jdk.javaapi

import java.util.concurrent.{CompletableFuture, CompletionStage}
import scala.concurrent.impl.FutureConvertersImpl.{CF, P}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

/** This object contains methods that convert between Scala [[scala.concurrent.Future]] and Java [[java.util.concurrent.CompletionStage]].
  *
  * The explicit conversion methods defined here are intended to be used in Java code. For Scala
  * code, it is recommended to use the extension methods defined in [[scala.jdk.FutureConverters]].
  *
  * Note that the bridge is implemented at the read-only side of asynchronous handles, namely
  * [[scala.concurrent.Future]] (instead of [[scala.concurrent.Promise]]) and [[java.util.concurrent.CompletionStage]] (instead of
  * [[java.util.concurrent.CompletableFuture]]). This is intentional, as the semantics of bridging
  * the write-handles would be prone to race conditions; if both ends (`CompletableFuture` and
  * `Promise`) are completed independently at the same time, they may contain different values
  * afterwards. For this reason, `toCompletableFuture` is not supported on the created
  * `CompletionStage`s.
  */
object FutureConverters {
  /** Returns a [[java.util.concurrent.CompletionStage]] that will be completed with the same value or exception as the
    * given Scala [[scala.concurrent.Future]] when that completes. Since the Future is a read-only representation,
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
  def asJava[T](f: Future[T]): CompletionStage[T] = {
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

  /** Returns a Scala [[scala.concurrent.Future]] that will be completed with the same value or exception as the
    * given [[java.util.concurrent.CompletionStage]] when that completes. Transformations of the returned Future are
    * executed asynchronously as specified by the ExecutionContext that is given to the combinator
    * methods.
    *
    * @param cs The CompletionStage which may eventually supply the completion for the returned
    *           Scala Future
    * @return a Scala Future that represents the CompletionStage's completion
    */
  def asScala[T](cs: CompletionStage[T]): Future[T] = {
    cs match {
      case cf: CF[T] => cf.wrapped
      // in theory not safe (could be `class C extends Future[A] with CompletionStage[B]`):
      case f: Future[T @unchecked] => f
      case _ =>
        val p = new P[T](cs)
        val completedCF = cs match {
          case cf0: CompletableFuture[T @unchecked] =>
            // drop `MinimalStage` (scala/bug#12918)
            val cf = cf0.toCompletableFuture
            if (cf.isDone && !cf.isCompletedExceptionally) cf else null
          case _ => null
        }
        if (completedCF != null)
          p.tryComplete(Success(completedCF.join()))
        else
          cs.handle(p)
        p.future
    }
  }
}
