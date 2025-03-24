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

import scala.annotation.compileTimeOnly
import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.tools.testkit.async.AsyncStateMachine

object OutputAwait {
  def writing[T](body: T): Output[T] = macro impl
  @compileTimeOnly("[async] `value` must be enclosed in `writing`")
  def value[T](output: Output[T]): T = ???
  def impl(c: blackbox.Context)(body: c.Tree): c.Tree = {
    import c.universe._
    val awaitSym = typeOf[OutputAwait.type].decl(TermName("value"))
    def mark(t: DefDef): Tree = c.internal.markForAsyncTransform(c.internal.enclosingOwner, t, awaitSym, Map.empty)
    val name = TypeName("stateMachine$async")
    q"""
      final class $name extends _root_.scala.tools.partest.async.OutputStateMachine {
        ${mark(q"""override def apply(tr$$async: _root_.scala.Option[_root_.scala.AnyRef]) = ${body}""")}
      }
      new $name().start().asInstanceOf[${c.macroApplication.tpe}]
    """
  }
}

case class Output[T](value: Option[T], written: HashMap[String, Vector[Any]])
object Output {
  def apply[T](value: T, written: (String, Any)*): Output[T] = {
    new Output(Some(value), toMultiMap[String, Any](written))
  }

  def mergeMultiMap[K, V](m1: HashMap[K, Vector[V]], m2: HashMap[K, Vector[V]]): HashMap[K, Vector[V]] = {
    m1.merged(m2) {
      case ((k1, v1), (k2, v2)) => (k1, v1 ++ v2)
    }
  }

  private def toMultiMap[K, V](written: Seq[(K, V)]): HashMap[K, Vector[V]] = {
    val mutableMap = collection.mutable.HashMap[K, mutable.Builder[V, Vector[V]]]()
    for ((k, v) <- written) mutableMap.getOrElseUpdate(k, Vector.newBuilder[V]) += v
    val immutableMapBuilder = collection.immutable.HashMap.newBuilder[K, Vector[V]]
    immutableMapBuilder ++= mutableMap.view.mapValues(_.result())
    immutableMapBuilder.result()
  }
}

abstract class OutputStateMachine extends AsyncStateMachine[Output[AnyRef], Option[AnyRef]] {
  private var written = collection.immutable.HashMap[String, Vector[Any]]()
  var result$async: Output[AnyRef] = _

  // FSM translated method
  def apply(tr$async: Option[AnyRef]): Unit

  // Required methods
  private[this] var state$async: Int = 0
  protected def state: Int = state$async
  protected def state_=(s: Int): Unit = state$async = s
  protected def completeFailure(t: Throwable): Unit = throw t
  protected def completeSuccess(value: AnyRef): Unit = result$async = Output(Some(value), written)
  protected def onComplete(f: Output[AnyRef]): Unit = ???
  protected def getCompleted(f: Output[AnyRef]): Option[AnyRef] = {
    written = Output.mergeMultiMap(written, f.written)
    f.value
  }
  protected def tryGet(tr: Option[AnyRef]): AnyRef = tr match {
    case Some(value) =>
      value.asInstanceOf[AnyRef]
    case None =>
      result$async = Output(None, written)
      this // sentinel value to indicate the dispatch loop should exit.
  }
  def start(): Output[AnyRef] = {
    apply(None)
    result$async
  }
}

