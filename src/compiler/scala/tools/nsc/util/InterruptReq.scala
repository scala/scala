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

package scala.tools.nsc
package util

/** A class of work items to be used in interrupt requests.
 *  Todo: we should replace the Eithers by Futures or Try's.
 */
abstract class InterruptReq {
  /** The result type of the operation
   */
  type R

  /** The operation to be performed */
  protected val todo: () => R

  type Continuation = Either[R, Throwable] => Unit

  /** The result provided */
  private var result: Option[Either[R, Throwable]] = None

  /** The continuations waiting asynchronously on a provided result */
  private var waiting: List[Continuation] = Nil

  /** To be called from interrupted server to execute demanded task */
  def execute(): Unit = synchronized {
    try {
      result = Some(Left(todo()))
    } catch {
      case t: Throwable => result = Some(Right(t))
    } finally {
      notify()
      for (k <- waiting.reverse) k(result.get)
    }
  }

  /** To be called from interrupting client to get result for interrupt */
  def getResult(): R = synchronized {
    while (result.isEmpty) {
      try {
        wait()
      } catch { case _ : InterruptedException => () }
    }

    result.get match {
      case Left(res) => res
      case Right(t) => throw new FailedInterrupt(t)
    }
  }

  def onComplete(k: Continuation) = synchronized {
    if (result.isDefined)
      k(result.get)
    else
      waiting = k :: waiting
  }
}

class FailedInterrupt(cause: Throwable) extends Exception("Compiler exception during call to 'ask'", cause)
