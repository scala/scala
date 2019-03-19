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

package scala.concurrent.impl

import java.util.concurrent.{CompletableFuture, CompletionStage}
import java.util.function.BiConsumer

import scala.concurrent.Future
import scala.concurrent.impl.Promise.DefaultPromise
import scala.util.{Failure, Success, Try}

object FutureConvertersImpl {
  class CF[T](val wrapped: Future[T]) extends CompletableFuture[T] with (Try[T] => Unit) {
    override def apply(t: Try[T]): Unit = t match {
      case Success(v) ⇒ complete(v)
      case Failure(e) ⇒ completeExceptionally(e)
    }
  }

  class P[T](val wrapped: CompletionStage[T]) extends DefaultPromise[T] with BiConsumer[T, Throwable] {
    override def accept(v: T, e: Throwable): Unit = {
      if (e == null) complete(Success(v))
      else complete(Failure(e))
    }
  }
}

