/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

private[actors] trait Combinators {

  implicit def mkBody[a](body: => a): Actor.Body[a]

  /**
   * Causes <code>self</code> to repeatedly execute
   * <code>body</code>.
   *
   * @param body the code block to be executed
   */
  def loop(body: => Unit): Unit = body andThen loop(body)

  /**
   * Causes <code>self</code> to repeatedly execute
   * <code>body</code> while the condition
   * <code>cond</code> is <code>true</code>.
   *
   * @param cond the condition to test
   * @param body the code block to be executed
   */
  def loopWhile(cond: => Boolean)(body: => Unit): Unit =
    if (cond) { body andThen loopWhile(cond)(body) }
    else continue

  def continue: Unit = throw new KillActorControl

}
