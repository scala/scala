/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors

/** <p>
 *    The class <code>ActorTask</code>.
 *  </p>
 *
 *  @author Philipp Haller
 */
private[actors] class ActorTask(actor: Actor, fun: () => Unit) extends ReactorTask[Actor](actor, fun) {

  protected override def beforeExecuting() {
    if (actor.shouldExit)
      actor.exit()
  }

  protected override def afterExecuting(e: Exception) {
    actor.synchronized {
      if (!actor.links.isEmpty)
        actor.exitLinked(e)
    }
  }

}
