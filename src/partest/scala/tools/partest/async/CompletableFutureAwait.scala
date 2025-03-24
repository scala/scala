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

package scala.tools.partest.async

import java.util.Objects
import java.util.concurrent.{CompletableFuture, Executor}
import java.util.function.BiConsumer

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.tools.testkit.async.AsyncStateMachine
import scala.util.{Failure, Success, Try}

object CompletableFutureAwait {
  def async[T](executor: Executor)(body: T): CompletableFuture[T] = macro impl
  @compileTimeOnly("[async] `await` must be enclosed in `async`")
  def await[T](completableFuture: CompletableFuture[T]): T = ???
  def impl(c: blackbox.Context)(executor: c.Tree)(body: c.Tree): c.Tree = {
    import c.universe._
    val awaitSym = typeOf[CompletableFutureAwait.type].decl(TermName("await"))
    def mark(t: DefDef): Tree = c.internal.markForAsyncTransform(c.internal.enclosingOwner, t, awaitSym, Map.empty)
    val name = TypeName("stateMachine$async")
    q"""
      final class $name extends _root_.scala.tools.partest.async.CompletableFutureStateMachine($executor) {
        ${mark(q"""override def apply(tr$$async: _root_.scala.util.Try[_root_.scala.AnyRef]) = ${body}""")}
      }
      new $name().start().asInstanceOf[${c.macroApplication.tpe}]
    """
  }
}

abstract class CompletableFutureStateMachine(executor: Executor) extends AsyncStateMachine[CompletableFuture[AnyRef], Try[AnyRef]] with Runnable with BiConsumer[AnyRef, Throwable] {
  Objects.requireNonNull(executor)

  protected var result$async: CompletableFuture[AnyRef] = new CompletableFuture[AnyRef]();

  // Adapters
  def accept(value: AnyRef, throwable: Throwable): Unit = {
    this(if (throwable != null) Failure(throwable) else Success(value))
  }
  def run(): Unit = {
    apply(null)
  }

  // FSM translated method
  def apply(tr$async: Try[AnyRef]): Unit

  // Required methods
  private[this] var state$async: Int = 0
  protected def state: Int = state$async
  protected def state_=(s: Int): Unit = state$async = s
  protected def completeFailure(t: Throwable): Unit = result$async.completeExceptionally(t)
  protected def completeSuccess(value: AnyRef): Unit = result$async.complete(value)
  protected def onComplete(f: CompletableFuture[AnyRef]): Unit = f.whenCompleteAsync(this)
  protected def getCompleted(f: CompletableFuture[AnyRef]): Try[AnyRef] = try {
    val r = f.getNow(this)
    if (r == this) null
    else Success(r)
  } catch {
    case t: Throwable => Failure(t)
  }
  protected def tryGet(tr: Try[AnyRef]): AnyRef = tr match {
    case Success(value) =>
      value.asInstanceOf[AnyRef]
    case Failure(throwable) =>
      result$async.completeExceptionally(throwable)
      this // sentinel value to indicate the dispatch loop should exit.
  }
  def start(): CompletableFuture[AnyRef] = {
    executor.execute(this)
    result$async
  }
}
