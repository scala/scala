/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors


import java.lang.Thread

/**
 * This class provides a dynamic actor proxy for normal Java
 * threads.
 *
 * @version 0.9.0
 * @author Philipp Haller
 */
private[actors] class ActorProxy(t: Thread) extends Actor {
  def act(): Unit = {}
  /**
   Terminates execution of <code>self</code> with the following
   effect on linked actors:

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>true</code>, send message
   <code>Exit(self, reason)</code> to <code>a</code>.

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>false</code> (default),
   call <code>a.exit(reason)</code> if
   <code>!reason.equals("normal")</code>.
   */
  override def exit(reason: String): Unit = {
    exitReason = reason
    t.interrupt()
  }
}
