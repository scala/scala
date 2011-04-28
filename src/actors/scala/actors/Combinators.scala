/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

private[actors] trait Combinators {

  /**
   * Enables the composition of suspendable closures using `andThen`,
   * `loop`, `loopWhile`, etc.
   */
  implicit def mkBody[a](body: => a): Actor.Body[a]

  /**
   * Repeatedly executes `body`.
   *
   * @param body the block to be executed
   */
  def loop(body: => Unit): Unit = body andThen loop(body)

  /**
   * Repeatedly executes `body` while the condition `cond` is `true`.
   *
   * @param cond the condition to test
   * @param body the block to be executed
   */
  def loopWhile(cond: => Boolean)(body: => Unit): Unit =
    if (cond) { body andThen loopWhile(cond)(body) }
    else continue

  /**
   * Continues with the execution of the closure registered as
   * continuation following `andThen`. Continues with the execution
   * of the next loop iteration when invoked inside the body of `loop`
   * or `loopWhile`.
   */
  def continue(): Unit = throw new KillActorControl

}
