/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors


import java.lang.Thread

/**
 * The class <code>ActorProxy</code>provides a dynamic actor proxy for normal
 * Java threads.
 *
 * @version 0.9.4
 * @author Philipp Haller
 */
private[actors] class ActorProxy(t: Thread) extends Actor {

  def act() {}

  /**
   * <p>
   *   Terminates execution of <code>self</code> with the following
   *   effect on linked actors:
   * </p>
   * <p>
   *   For each linked actor <code>a</code> with
   *   <code>trapExit</code> set to <code>true</code>, send message
   *   <code>{'EXIT, self, reason}</code> to <code>a</code>.
   * </p>
   * <p>
   *   For each linked actor <code>a</code> with
   *   <code>trapExit</code> set to <code>false</code> (default),
   *   call <code>a.exit(reason)</code> if
   *   <code>reason != 'normal</code>.
   * </p>
   *
   * @param reason the exit reason of the interrupted thread.
   */
  override def exit(reason: AnyRef): Nothing = {
    kill()
    // links
    if (!links.isEmpty) {
      exitReason = reason
      exitLinked()
    }
    throw new InterruptedException
  }
}
