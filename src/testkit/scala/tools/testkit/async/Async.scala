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

import java.util.Objects

import scala.annotation.compileTimeOnly
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.util.{Failure, Success, Try}

object Async {
  def async[T](body: T)(implicit executionContext: ExecutionContext): Future[T] = macro impl
  @compileTimeOnly("[async] `await` must be enclosed in an `async` block")
  def await[T](completableFuture: Future[T]): T = ???

  def impl(c: blackbox.Context)(body: c.Tree)(executionContext: c.Tree): c.Tree = {
    import c.universe._
    val awaitSym = typeOf[Async.type].decl(TermName("await"))
    def mark(t: DefDef): Tree = {
      c.internal.markForAsyncTransform(c.internal.enclosingOwner, t, awaitSym, Map.empty)
    }
    val name = TypeName("stateMachine$async")
    q"""
      final class $name extends _root_.scala.tools.testkit.async.AsyncAsMacroStateMachine($executionContext) {
        ${mark(q"""override def apply(tr$$async: _root_.scala.util.Try[_root_.scala.AnyRef]) = ${body}""")}
      }
      new $name().start().asInstanceOf[${c.macroApplication.tpe}]
    """
  }
}

abstract class AsyncAsMacroStateMachine(execContext: ExecutionContext) extends AsyncStateMachine[Future[AnyRef], Try[AnyRef]] with Function1[Try[AnyRef], Unit] {
  Objects.requireNonNull(execContext)

  private val result$async: Promise[AnyRef] = Promise[AnyRef]()

  // FSM translated method
  def apply(tr$async: Try[AnyRef]): Unit

  // Required methods
  private[this] var state$async: Int = 0
  protected def state: Int = state$async
  protected def state_=(s: Int): Unit = state$async = s

  // scala-async accidentally started catching NonFatal exceptions in:
  //  https://github.com/scala/scala-async/commit/e3ff0382ae4e015fc69da8335450718951714982#diff-136ab0b6ecaee5d240cd109e2b17ccb2R411
  // This follows the new behaviour but should we fix the regression?
  protected def completeFailure(t: Throwable): Unit = result$async.complete(Failure(t))

  protected def completeSuccess(value: AnyRef): Unit = result$async.complete(Success(value))
  protected def onComplete(f: Future[AnyRef]): Unit = f.onComplete(this)(execContext)
  protected def getCompleted(f: Future[AnyRef]): Try[AnyRef] = {
    if (f.isCompleted) {
      f.value.get
    } else null
  }
  protected def tryGet(tr: Try[AnyRef]): AnyRef = tr match {
    case Success(value) =>
      value.asInstanceOf[AnyRef]
    case Failure(throwable) =>
      completeFailure(throwable)
      this // sentinel value to indicate the dispatch loop should exit.
  }
  def start(): Future[AnyRef] = {
    // This cast is safe because we know that `def apply` does not consult its argument when `state == 0`.
    Future.unit.asInstanceOf[Future[AnyRef]].onComplete(this)(execContext)
    result$async.future
  }
}
