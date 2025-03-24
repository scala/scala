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

package scala.tools.partest
package async

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.tools.testkit.async.AsyncStateMachine

object OptionAwait {
  def optionally[T](body: T): Option[T] = macro impl
  @compileTimeOnly("[async] `value` must be enclosed in `optionally`")
  def value[T](option: Option[T]): T = ???
  def impl(c: blackbox.Context)(body: c.Tree): c.Tree = {
    import c.universe._
    val awaitSym = typeOf[OptionAwait.type].decl(TermName("value"))
    def mark(t: DefDef): Tree = c.internal.markForAsyncTransform(c.internal.enclosingOwner, t, awaitSym, Map.empty)
    val name = TypeName("stateMachine$async")
    q"""
      final class $name extends _root_.scala.tools.partest.async.OptionStateMachine {
        ${mark(q"""override def apply(tr$$async: _root_.scala.Option[_root_.scala.AnyRef]) = ${body}""")}
      }
      new $name().start().asInstanceOf[${c.macroApplication.tpe}]
    """
  }
}

abstract class OptionStateMachine extends AsyncStateMachine[Option[AnyRef], Option[AnyRef]] {
  var result$async: Option[AnyRef] = _

  // FSM translated method
  def apply(tr$async: Option[AnyRef]): Unit

  // Required methods
  private[this] var state$async: Int = 0
  protected def state: Int = state$async
  protected def state_=(s: Int): Unit = state$async = s
  protected def completeFailure(t: Throwable): Unit = throw t
  protected def completeSuccess(value: AnyRef): Unit = result$async = Some(value)
  protected def onComplete(f: Option[AnyRef]): Unit = ???
  protected def getCompleted(f: Option[AnyRef]): Option[AnyRef] = {
    f
  }
  protected def tryGet(tr: Option[AnyRef]): AnyRef = tr match {
    case Some(value) =>
      value.asInstanceOf[AnyRef]
    case None =>
      result$async = None
      this // sentinel value to indicate the dispatch loop should exit.
  }
  def start(): Option[AnyRef] = {
    apply(None)
    result$async
  }
}

